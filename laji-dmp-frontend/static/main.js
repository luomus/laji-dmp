import { Elm } from '../src/Main.elm'

var storedData = localStorage.getItem('login');
let flags = storedData ? JSON.parse(storedData) : null;
var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags
});

app.ports.updateLocalStorage.subscribe(message => {
  localStorage.setItem("login", JSON.stringify(message));
});
app.ports.toggleDialog.subscribe(id => {
  const dialog = document.querySelector(`#${id}`)
  if (dialog.open) {
    dialog.close();
  } else {
    dialog.showModal();
  }
});
