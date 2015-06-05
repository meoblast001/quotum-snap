<style>
  .navbar-quotum {
    margin-bottom: 0px;
    border-bottom: 1px solid #00578E;
   }
</style>

<div class="splash">
  <div class="container">
    <h1>Log in</h1>

    <p><loginError/></p>

    <bind tag="postAction">/login</bind>
    <bind tag="submitText">Login</bind>
    <apply template="userform"/>

    <p>Don't have a login yet? <a href="/new_user">Create a new user</a></p>
  </div>
</div>
