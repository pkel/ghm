<?php

function print_app() { ?>
<!DOCTYPE html>
<html lang="de">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <link rel="stylesheet" href="assets/css/bootstrap.min.css">
    <link rel="stylesheet" href="assets/css/font-awesome-all.min.css">
    <script type='text/javascript' src='app.js'></script>
  </head>
  <body>
    <div class="container-fluid">
      <div id="ghm_main">Lädt ...</div>
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
  </body>
</html>
<?php }

function print_login() { ?>
<!DOCTYPE html>
<html lang="de">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <link rel="stylesheet" href="assets/css/bootstrap.min.css">
  </head>
  <body>
    <div class="container-fluid">
      <div class="row justify-content-center align-items-center" style="height:100vh">
        <div class="col-4">
          <div class="card">
            <div class="card-body">
              <form action="." method="POST" autocomplete="off">
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
</html>
<?php }

session_start();

function fail ($status) {
  http_response_code ($status);
  exit();
}

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
    $dsn = 'pgsql:host=' . $host . ';port=' . $port . ';dbname=' . $name
      . ';user=' . $user . ';password=' . $pass;
    $pdo = new PDO($dsn);
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
    $__pdo = $pdo;
    return $pdo;
  }
}

function redirect_get($status="") {
  // TODO: preserve other parameters
  if (strlen($status) > 0) {
    $param = "?status=".$status;
  } else {
    $param = "" ;
  }
  http_response_code(303);
  header("Location: " . $_SERVER['REQUEST_URI'] . $param);
  exit();
}

function login($username, $password){
  $query = "SELECT auth.user_role(:id, :pass)";
  $stmt = pdo()->prepare($query);
  $stmt->execute(array(':id' => $username, ':pass' => $password));
  $role = $stmt->fetch()[0];
  if ($role === NULL) {
    redirect_get("login-fail");
  } else {
    $_SESSION['user']=$username;
    $_SESSION['role']=$role;
    redirect_get();
  }
}

function logout() {
  unset($_SESSION['user']);
  unset($_SESSION['role']);
  redirect_get('logout-success');
}

if ($_SERVER['REQUEST_METHOD'] === 'POST') {
  // sanitize input
  $filter_def = array(
    'action'   => FILTER_SANITIZE_STRING,
    'username' => FILTER_SANITIZE_STRING,
    'password' => FILTER_DEFAULT);
  $post = filter_var_array($_POST, $filter_def);
  // act
  switch($post['action']){
  case 'login':
    login(trim($post['username']), $post['password']);
    break;
  case 'logout':
    logout();
    break;
  default:
    fail(400);
  }
} elseif ($_SERVER['REQUEST_METHOD'] === 'GET') {
  // TODO: if logged in print_app else
  if (isset($_SESSION['role'])) {
    print_app ();
  } else {
    print_login ();
  }
} else {
  fail(405);
}

?>
