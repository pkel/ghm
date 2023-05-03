<?php

ini_set('session.name', 'ghm_session');
ini_set('session.use_only_cookies', 1);
ini_set('session.cookie_httponly', 1);
session_start();

// open database connection
function pdo() {
  global $__pdo;
  if (isset($__pdo)) {
    return $__pdo;
  } else {
    $host = getenv('DB_HOST');
    $port = getenv('DB_PORT');
    $pass = getenv('DB_PASS');
    $user = getenv('DB_USER');
    $name = getenv('DB_NAME');
    $dsn = 'pgsql:host=' . $host . ';port=' . $port . ';dbname=' . $name;
    $pdo = new PDO($dsn, $user, $pass);
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
    $__pdo = $pdo;
    return $pdo;
  }
}

// set response code and exit
function fail ($status) {
  http_response_code ($status);
  exit();
}

function succeed ($status=200) {
  http_response_code ($status);
  exit();
}

// redirect get $path?status=?status
function redirect_get($path, $status="") {
  // TODO: preserve other parameters
  if (strlen($status) > 0) {
    $param = "?status=".$status;
  } else {
    $param = "" ;
  }
  http_response_code(303);
  header("Location: " . $path . $param);
  exit();
}

function logout() {
  $_SESSION = array();
  redirect_get('/', 'logout-success');
}

function login($username, $password, $fragment, $headless=false){
  $query = "SELECT auth.user_role(:id, :pass)";
  $stmt = pdo()->prepare($query);
  $stmt->execute(array(':id' => $username, ':pass' => $password));
  $role = $stmt->fetch()[0];
  if ($role === NULL) {
    if ($headless) {
      fail(401);
    } else {
      redirect_get('/', "login-fail");
    }
  } else {
    $_SESSION['user']=$username;
    $_SESSION['role']=$role;
    if ($headless) {
      token();
    } else {
      redirect_get('/' . $fragment);
    }
  }
}

function token(){
  if (isset($_SESSION['role'])) {
    $query = "SELECT auth.token(:id, :role, :secret)";
    $stmt = pdo()->prepare($query);
    $stmt->execute(array(
      ':id' => $_SESSION['user'],
      ':role' => $_SESSION['role'],
      ':secret' => getenv('JWT_SECRET')
    ));
    $token = $stmt->fetch()[0];
    if ($token === NULL) {
      fail(400);
    } else {
      echo($token);
      succeed();
    }
  } else {
    fail(401);
  }
}

function print_app($user) { ?>
<!DOCTYPE html>
<html lang="de">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <link rel="stylesheet" href="assets/css/bootstrap.min.css">
    <link rel="stylesheet" href="assets/css/font-awesome-all.min.css">
    <link rel="stylesheet" href="assets/css/app.css">
  </head>
  <body>
    <div class="container-fluid">
      <div class="headline justify-content-end row">
        <div class="col-auto">Angemeldet als <b><?php print ($user)?></b>.
          <a href="?action=logout">Abmelden.</a>
        </div>
      </div>
      <div id="ghm_main">
        <div class="row justify-content-center align-items-center" style="height:100vh">
          <div class="col-auto">
            <div class="spinner-border" role="status">
              <span class="sr-only">Lädt...</span>
            </div>
          </div>
        </div>
      </div>
    </div>
    <script type='text/javascript' src='assets/js/jquery-3.3.1.slim.min.js'></script>
    <script type='text/javascript' src='assets/js/bootstrap.bundle.min.js'></script>
    <script type='text/javascript' src='assets/js/clipboard.min.js'></script>
    <script type='text/javascript'>
      var clipboard = new ClipboardJS('.clipboard-js');
      clipboard.on('error', function(e) {
        console.log(e);
      });
    </script>
    <script async type='text/javascript' src='app.js'></script>
  </body>
</html>
<?php }

function print_alert_box($class, $msg) { ?>
<div class="alert alert-<?php print($class) ?>" role="alert">
  <?php print($msg . "\n") ?>
</div>
<?php }

function print_status($status) {
  switch ($status) {
  case "login-fail":
    print_alert_box("danger", "Nutzername ungültig oder Passwort falsch");
    break;
  case "logout-success":
    print_alert_box("info", "Erfolgreich abgemeldet");
    break;
  }
}

function print_login($status) { ?>
<!DOCTYPE html>
<html lang="de">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <link rel="stylesheet" href="assets/css/bootstrap.min.css">
    <script>
// Get parameters are only for communicating status message. Drop.
window.history.replaceState({}, document.title, window.location.href.split('?')[0]);
    </script>
  </head>
  <body>
    <div class="container-fluid">
      <div class="row justify-content-center align-items-center" style="height:100vh">
        <div class="col-4">
<?php print_status($status) ?>
          <div class="card">
            <div class="card-body">
              <form id="login-form" action="." method="POST" autocomplete="off">
                <div class="form-group">
                  <input type="text" class="form-control" name="username" placeholder="Nutzername" required>
                </div>
                <div class="form-group">
                  <input type="password" class="form-control" name="password" placeholder="Passwort" required>
                </div>
                <button class="btn btn-primary" name="action" value="login">Anmelden</button>
              </form>
            </div>
          </div>
        </div>
      </div>
    </div>
  </body>
  <script type='text/javascript'>
    // submit url fragment (with hash) to server
    var form = document.getElementById('login-form');
    form.addEventListener('submit',
    function(){
        var hidden = document.createElement("input");
        hidden.setAttribute('type','hidden');
        hidden.setAttribute('name','fragment');
        hidden.setAttribute('value',window.location.hash);
        this.appendChild(hidden);
    });
  </script>
</html>
<?php }

if ($_SERVER['REQUEST_METHOD'] === 'POST') {
  // sanitize input
  $filter_def = array(
    'action'   => FILTER_SANITIZE_STRING,
    'username' => FILTER_SANITIZE_STRING,
    'password' => FILTER_DEFAULT,
    'fragment' => FILTER_DEFAULT);
  $post = filter_var_array($_POST, $filter_def);
  // act
  switch($post['action']){
  case 'login':
    login(trim($post['username']), $post['password'], $post['fragment'], false);
    break;
  case 'token':
    login(trim($post['username']), $post['password'], '', true);
    break;
  case 'logout':
    logout();
    break;
  default:
    fail(400);
  }
} elseif ($_SERVER['REQUEST_METHOD'] === 'GET') {
  $filter_def = array(
    'status' => FILTER_SANITIZE_STRING,
    'action' => FILTER_SANITIZE_STRING);
  $get = filter_var_array($_GET, $filter_def);
  switch($get['action']){
  case 'token':
    token();
    break;
  case 'logout':
    logout();
    break;
  default:
    if (isset($_SESSION['role'])) {
      print_app ($_SESSION['user']);
    } else {
      print_login ($get['status']);
    }
  }
} else {
  fail(405);
}

?>
