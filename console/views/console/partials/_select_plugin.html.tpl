<script>
$('.plugin-select').on('click', function(e) {
  var all = $('#plugins').val()
  if (all.trim().length == 0) {
    $('#plugins').val(this.name)
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
  $('#plugins').val(tags.join(' '))
})
</script>
