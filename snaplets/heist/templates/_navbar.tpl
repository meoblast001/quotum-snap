<nav class="navbar navbar-quotum">
  <div class="container">
    <div class="navbar-header">
      <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
      <a class="navbar-brand" href="/">
        Quotum
      </a>
    </div>

    <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
      <ul class="nav navbar-nav navbar-right">
        <ifLoggedIn>
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">
              <loggedInUser/> <span class="caret"></span>
            </a>
            <ul class="dropdown-menu" role="menu">
              <li><a href="#">Add a Quote</a></li>
              <li><a href="/category">Suggest a Category</a></li>
              <li class="divider"></li>
              <li><a href="#">Edit Profile</a></li>
              <li><a href="/logout">Log Out</a></li>
            </ul>
          </li>
        </ifLoggedIn>
        <ifLoggedOut>
          <li><a href="/new_user">New User?</a></li>
          <li><a href="/login">Log In</a></li>
        </ifLoggedOut>
      </ul>
    </div>
  </div>
</nav>
