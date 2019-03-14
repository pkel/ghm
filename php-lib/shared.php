<?php

ini_set('session.name', 'ghm_session');
ini_set('session.use_only_cookies', 1);
ini_set('session.cookie_httponly', 1);
session_start();

// set response code and exit
function fail ($status) {
  http_response_code ($status);
  exit();
}

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
    $dsn = 'pgsql:host=' . $host . ';port=' . $port . ';dbname=' . $name
      . ';user=' . $user . ';password=' . $pass;
    $pdo = new PDO($dsn);
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
    $__pdo = $pdo;
    return $pdo;
  }
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

function login($username, $password){
  $query = "SELECT auth.user_role(:id, :pass)";
  $stmt = pdo()->prepare($query);
  $stmt->execute(array(':id' => $username, ':pass' => $password));
  $role = $stmt->fetch()[0];
  if ($role === NULL) {
    redirect_get('/', "login-fail");
  } else {
    $_SESSION['user']=$username;
    $_SESSION['role']=$role;
    redirect_get('/');
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
    }
  } else {
    fail(401);
  }
}

?>
