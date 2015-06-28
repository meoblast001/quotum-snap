<apply template="base">

  <ifLoggedIn>
    <div class="container t10">
      <p>Congrats! You're logged in as '<loggedInUser/>'</p>
      <p><a href="/logout">Logout</a></p>
    </div>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

  <hr />

  <div class="container">
    <div class="row">
      <div class="col-xs-6 col-md-4">
        <h1>Stuff here</h1>
        <p>
          And maybe a paragraph about what Quotum is.
        </p>
      </div>

      <div class="col-xs-6 col-md-4">
        <h1>Stuff here</h1>
        <p>
          And maybe a paragraph about what Quotum is.
        </p>
      </div>

      <div class="col-xs-6 col-md-4">
        <h1>Stuff here</h1>
        <p>
          And maybe a paragraph about what Quotum is.
        </p>
      </div>
    </div>
  </div>

</apply>
