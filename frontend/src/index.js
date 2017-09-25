import './main.css';
import { Main } from './Main.elm';

const flags = {
  username: window.prompt("What's your username?")
}

Main.embed(document.getElementById('root'), flags);
