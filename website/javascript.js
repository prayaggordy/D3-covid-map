function toggle(id, className, visibleClassName) {
    const elements = document.getElementsByClassName(className);
    Array.from(elements).forEach(element => {
        if (element.id !== id) {
            element.classList.remove(visibleClassName);
            document.getElementById(element.id + "Button").classList.remove("engaged");
        } else {
            element.classList.add(visibleClassName);
            console.log(element.id + "Button");
            document.getElementById(element.id + "Button").classList.add("engaged");
        }
    });
}
function toggleMap(id) {
    toggle(id, "toggleMap", "toggleMapVisible")
}
function toggleLine(id) {
    toggle(id, "toggleLine", "toggleLineVisible")
}
function toggleBar(id) {
    toggle(id, "toggleBar", "toggleBarVisible")
}
function toggleTable(id) {
    toggle(id, "toggleTable", "toggleTableVisible")
}
