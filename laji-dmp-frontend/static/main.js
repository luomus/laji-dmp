import { Elm } from '../src/Main.elm';

var login = localStorage.getItem('login');
let dmpApiBase = import.meta.env.VITE_DMP_API_BASE || "http://localhost:4000";
let lajiApiBase = import.meta.env.VITE_LAJI_API_BASE || "https://dev.laji.fi/api";
let authUrl = import.meta.env.VITE_AUTH_URL || "https://fmnh-ws-test.it.helsinki.fi/laji-auth/login?target=KE.1661&redirectMethod=GET&locale=fi&next=";
let flags = {
  login: login ? JSON.parse(login) : null,
  dmpApiBase,
  lajiApiBase,
  authUrl
};
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

