var fields = ['sender','address','subject','sidebar','body','attachments'];

function fieldId (fld) {
  return "editable-" + fld;
}

function getState () {
  var state = { };
  fields.forEach(function (el) {
    state[el] = document.getElementById(fieldId(el)).innerHTML;
  });
  return state;
}

function setState (state) {
  fields.forEach(function (el) {
    document.getElementById(fieldId(el)).innerHTML = state[el];
  });
}

// safe content to hash for bookmarking
var lastSave
function safe () {
  lastSave = btoa(JSON.stringify(getState()));
  document.location.hash = lastSave;
}

// load content from hash
function load () {
  var b64 = document.location.hash.substr(1);
  if (b64.length > 0 && lastSave != b64) {
    var state = JSON.parse(window.atob(b64));
    setState(state);
  }
}

function main () {
  // trigger safe after 1 seconds of no input
  var timer
  document.body.addEventListener("input", function() {
    window.clearTimeout(timer);
    timer = setTimeout(safe, 1000);

  }, false);

  // respect forward/backward navigation
  window.onhashchange = load;

  load ();
  safe ();
}

main ();
