document.addEventListener("DOMContentLoaded", function (event) {
    // code for the tabs in /registration.html

    // tabs & corresponding pages
    let types = ["intern", "ntu", "non-ntu", "sit-in"];

    let selected = "intern";

    types.forEach(function (type) {
        let tab = document.getElementById(type + "-tab");
        let page = document.getElementById(type);

        if (tab) {
            tab.addEventListener("click", function (event) {
                // remove `.selected`
                document.getElementById(selected + "-tab").classList.remove("selected");
                document.getElementById(selected).classList.remove("selected");
                // update the `selected` 
                selected = type;
                // insert `.selected`
                tab.classList.add("selected");
                page.classList.add("selected");
                // disable the <a> link
                event.preventDefault();
            })
        }
    });
});