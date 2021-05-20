import elmPlugin from "vite-plugin-elm"


module.exports = {
    plugins: [ elmPlugin() ],
    server: {
        proxy: {
            '/socket.io': {
                target: 'http://localhost:8080'
            }
        }
    }
}
