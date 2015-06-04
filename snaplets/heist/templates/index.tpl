<apply template="base">

  <ifLoggedIn>
    <p>Congrats! You're logged in as '<loggedInUser/>'</p>

    <p><a href="/logout">Logout</a></p>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
