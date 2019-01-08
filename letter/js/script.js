function get_el (id) {
  return document.getElementById(id).innerHTML
}

function set_el (id, html) {
  document.getElementById(id).innerHTML = html
}

function get_state () {
  var state = { };
  state.sender = get_el("editable-sender");
  state.address = get_el("editable-address");
  state.subject = get_el("editable-subject");
  state.sidebar = get_el("editable-sidebar");
  state.body = get_el("editable-body");
  state.attachments = get_el("editable-attachments");
  return state;
}

function set_state (state) {
  set_el("editable-sender" , state.sender );
  set_el("editable-address", state.address);
  set_el("editable-subject", state.subject);
  set_el("editable-sidebar", state.sidebar);
  set_el("editable-body"   , state.body   );
}

// safe content to hash for bookmarking
function safe () {
  document.location.hash = btoa(JSON.stringify(get_state()));
}

// trigger safe after 1 seconds of no input
var timer
document.body.addEventListener("input", function() {
  window.clearTimeout(timer);
  timer = setTimeout(safe, 1000);
}, false);

// init content from hash
var b64 = document.location.hash.substr(1);
if (b64.length > 0) {
  var state = JSON.parse(window.atob(b64));
  set_state(state);
}
