import * as Main from "../output/Main/index";

function main() {
    // environment variables go here (see spago docs)

    Main.main();
}

// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
    module.hot.accept(function () {
        console.log("Reloaded, running main again");
        main();
    });
}

console.log("Starting app");

main();
