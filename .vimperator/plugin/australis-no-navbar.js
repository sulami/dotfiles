(function() {

  "use strict";

  let hide_navbar = function (args) {
    Cu.import("resource:///modules/CustomizableUI.jsm");
    CustomizableUI.setToolbarVisibility(CustomizableUI.AREA_NAVBAR, false);
  };

  let show_navbar = function (args) {
    Cu.import("resource:///modules/CustomizableUI.jsm");
    CustomizableUI.setToolbarVisibility(CustomizableUI.AREA_NAVBAR, true);
  };

  commands.addUserCommand(["nonav[bar]"],
                          "Hide navbar",
                          args => hide_navbar(args),
                          { argCount: '0' });

  commands.addUserCommand(["nav[bar]"],
                          "Show navbar",
                          args => show_navbar(args),
                          { argCount: '0' });
})();
