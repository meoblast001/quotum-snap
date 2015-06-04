<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Quotum</title>
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css">
    <!--[if lt IE 9]>
        <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
        <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
    <![endif]-->
    <link href="http://fonts.googleapis.com/css?family=Architects+Daughter:400,700" rel="stylesheet">
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
    <style>
      .navbar {
        border-radius: 0px;
        background-color: #0084C8;
        border: 0px;
      }
      .navbar-quotum a { color: #fff; }
      .navbar-quotum a:hover { color: #ccc; }
      .navbar-brand { font-family: 'Architects Daughter', sans-serif; font-weight: bold; }

      .nav > li > a:focus, .nav > li > a:hover,
      .nav > li > a:focus, .nav > li > a:active { background-color: transparent; }

      .nav .open > a, .nav .open > a:focus, .nav .open > a:hover { background-color: #00578E; }
    </style>
  </head>
  <body>
    <apply template="_navbar"/>

    <div class="container">
      <apply-content/>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js"></script>
  </body>
</html>
