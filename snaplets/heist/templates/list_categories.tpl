<apply template="base">
  <table class="table table-striped">
    <tr>
      <th>Name</th>
      <th>URL</th>
      <th></th>
    </tr>
    <tr>
      <dfForm name="form" action="/category">
        <dfChildErrorList />
        <td><dfInputText ref="name" /></td>
        <td><dfInputText ref="slug" /></td>
        <td><dfInputSubmit value="Enter" /></td>
      </dfForm>
    </tr>
    <allQuoteCategories>
      <tr>
        <td><name/></td>
        <td><a href="/category/${slug}">/category/<slug/></a></td>
      </tr>
    </allQuoteCategories>
  </table>
</apply>
