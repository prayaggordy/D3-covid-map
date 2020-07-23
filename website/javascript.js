function toggle(id, className, visibleClassName) {
    const elements = document.getElementsByClassName(className);
    Array.from(elements).forEach(element => {
        if (element.id !== id) {
            element.classList.remove(visibleClassName);
        } else {
            element.classList.add(visibleClassName);
        }
    });
}
function toggleMap(id) {
    toggle(id, "toggleMap", "toggleMapVisible")
}