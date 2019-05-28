function domReady(fn) {
  document.addEventListener("DOMContentLoaded", fn);
  if (document.readyState === "interactive" || document.readyState === "complete" ) {
    fn();
  }
}

(domReady( function() { 
    var node = document.querySelector('body');
    Elm.Main.init({node:node}); 
} ));
