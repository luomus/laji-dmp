import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

export default defineConfig({
  server: {
    port: 8000,
  },
  plugins: [
    elmPlugin({
      debug: false,
      optimize: false
    })
  ],
});
