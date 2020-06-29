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
    var src;
    if ("picsDir" in params) {
        src = [params.picsDir, elem.pic].join("/");
    } else {
        src = elem.pic;
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

function main() {
    var layoutFile = File.openDialog("Select layout file");
    layoutFile.open("r");
    var layoutParams = JSONparse(layoutFile.read());
    layoutFile.close();
    var doc = app.documents[0];
    createLayout(doc, layoutParams, {"picsDir": layoutFile.path});
}

main();
