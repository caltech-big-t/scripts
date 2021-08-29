#targetengine "session"


function JSONparse(str) {
    return Function('"use strict";return ('+str+')')();
}

function boundsOf(elem) {
    var pos = elem.pos;
    var size = elem.size;
    return [
        pos[1],
        pos[0],
        pos[1] + size[1],
        pos[0] + size[0]
    ];
}

function createPic(page, elem, params) {
    var src = elem.pic;
    if (elem.fileset === "originals") {
        src = [params.pics, src].join("/");
    } else if (elem.fileset === "updates") {
        src = [params.updatedPics, src].join("/");
    }
    var pic = page.rectangles.add(
        params["layer"],
        undefined,
        undefined,
        {
            geometricBounds: boundsOf(elem),
            strokeWeight:0
        }
    );  
    pic.place(File(src));
    pic.fit(FitOptions.FILL_PROPORTIONALLY);
    return pic;
}

function createText(page, elem, params) {
    var text = page.textFrames.add(
        params["layer"],
        undefined,
        undefined,
        {geometricBounds: boundsOf(elem), contents: elem.txt}
    ); 
    if ("style" in params) {
        text.parentStory.texts.item(0).appliedParagraphStyle = params.style;
    }
    if ("align" in elem) {
        if (elem.align === "left") {
            text.paragraphs.item(0).justification = Justification.leftAlign;
        } else if (elem.align === "right") {
            text.paragraphs.item(0).justification = Justification.rightAlign;
        }
    }
    return text;
}

function createElem(page, elem, params) {
    if (elem.type === "pic") {
        return createPic(page, elem, params);
    } else if (elem.type === "text") {
        return createText(page, elem, params);
    } else {
        return null;
    }
}

function createLayout(doc, layout, params) {
    for (var i = 0; i < layout.pages.length; i ++ ) {
        var page = doc.pages.item(i);
        var layoutPage = layout.pages[i];
        for (var j = 0; j < layoutPage.elems.length; j ++) {
            createElem(page, layoutPage.elems[j], params);
        }
    }
}

function createFileSelect(root, props) {
    var selectButton = root.add("button", undefined, props.prompt);
    var selectedLabel = root.add("statictext", undefined, "No file selected");
    selectButton.onClick = function() {
        var selected = null;
        if (props.selectFolder) {
            if (props.defaultFolder) {
                selected = props.defaultFolder.selectDlg(props.prompt);
            } else {
                selected = Folder.selectDialog(props.prompt);
            }
        } else {
            selected = File.openDialog(props.prompt, props.filter, props.multiSelect);
        }
        if (selected === null) {
            return;
        }
        selectedLabel.text = selected.displayName;
        props.onSelect(selected);
    };
    return { 
        selectButton: selectButton,
        selectedLabel: selectedLabel,
        setDefaultFolder: function(folder) { props.defaultFolder = folder; }
    };
}


function createUI(props) {
    var dialog = new Window("window", "FacePalm");
    var panel = dialog.add("panel", undefined, "Select input files");
    var layoutSelect = createFileSelect(
        panel, { prompt: "Select layout", onSelect: props.onSelectedLayout }
    );
    var picsSelect = createFileSelect(
        panel, { prompt: "Select original photos", selectFolder: true, onSelect: props.onSelectedPics }
    );
    var updatedPicsSelect = createFileSelect(
        panel, { prompt: "Select updated photos", selectFolder: true, onSelect: props.onSelectedUpdatedPics }
    );
    var doLayoutButton = panel.add("button", undefined, "Do Layout");
    doLayoutButton.onClick = props.onDoLayout;
    var errorText = panel.add("statictext", undefined, "No errors");

    return {
        dialog: dialog,
        setErrorText: function(text) { errorText.text = text; },
        setDefaultFolder: function(folder) {
            picsSelect.setDefaultFolder(folder);
            updatedPicsSelect.setDefaultFolder(folder);
        }
    };
}

function doLayout(settings) {
    try{
        createLayout(app.documents[0], settings.layout, settings);
    } catch (err) {
        alert("Error while creating layout:\n" + err.toString());
    }
}

function main() {
    var settings = {
        layout: null,
        pics: null,
        updatedPics: null
    };
    var ui = createUI({
        onSelectedLayout: function(layoutFile) {
            try {
                layoutFile.open("r");
                var layoutParams = JSONparse(layoutFile.read());
                layoutFile.close();
                settings.layout = layoutParams;
                ui.setDefaultFolder(layoutFile.parent);
            } catch (err) {
                ui.setErrorText("Could not open layout file:\n" + err.toString());
            }
        },
        onSelectedPics: function (picsFolder) { settings.pics = picsFolder; },
        onSelectedUpdatedPics: function (updatedPicsFolder) { settings.updatedPics = updatedPicsFolder; },
        onDoLayout: function() { doLayout(settings); }
    });
    ui.dialog.show();
}

try {
    main();
} catch (err) {
    alert("Unknown error:\n", err);
}
