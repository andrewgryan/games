import elmPlugin from "vite-plugin-elm"


module.exports = {
    plugins: [ elmPlugin({ debug: false }) ],
    server: {
        proxy: {
            '/socket.io': {
                target: 'http://localhost:8080',
                changeOrigin: true,
                safe: false,
                ws: true
            }
        }
    }
}
