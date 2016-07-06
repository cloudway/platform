<style>
.plugin-select img {
  width: 20px;
  height: 20px;
  margin-right: 5px;
}
</style>

<script>
$('.plugin-select a').on('click', function(e) {
  var select = $(this).parent()
  var input = $(select.data('input'))
  var all = input.val()
  if (select.data('single-select') || all.trim().length == 0) {
    input.val(this.name)
    return
  }

  var tags = all.split(/\s+/)
  var new_tag = this.name
  var new_name = new_tag.split(':')[0]
  var found = false
  for (var i=0, len=tags.length; i<len; i++) {
    var name = tags[i].split(':')[0]
    if (name == new_name) {
      tags[i] = new_tag
      found = true
    }
  }
  if (!found) {
    tags.push(new_tag)
  }
  input.val(tags.join(' '))
})
</script>
