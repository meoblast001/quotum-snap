<dfForm name="login" action="${postAction}">
  <dfChildErrorList />
  <table id="info">
    <tr>
      <td>Login:</td><td><dfInputText ref="username" /></td>
    </tr>
    <tr>
      <td>Password:</td><td><dfInputPassword ref="password" /></td>
    </tr>
    <tr>
      <td>Remember me:</td><td><dfInputCheckbox ref="remember" /></td>
    </tr>
    <tr>
      <td></td>
      <td><dfInputSubmit value="${submitText}" /></td>
    </tr>
  </table>
</dfForm>
