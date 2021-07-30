var webpack = require("webpack");
var path = require('path');
var MiniCssExtractPlugin = require('mini-css-extract-plugin');

module.exports = {
    mode: "development",
    entry: {
        app: path.join(__dirname, '.', 'app.js')
    },
    externals: /(all|rts|lib|out|runmain).js$/,
    resolve: {
        extensions: ['.webpack.js', '.js', '.css', '.less', '.scss'],
        modules: ['node_modules']
    },
    devtool: 'source-map',
    output: {
        path: path.resolve(__dirname, './www-hakyll/css'),
        filename: '[name].js'
    },
    devServer: {
        contentBase: path.resolve(__dirname, "./www-hakyll/css"),
        compress: true,
        port: 8000
    },
    module: {
        noParse: /(all|rts|lib|out|runmain).js$/,
        rules: [{
            test: /\.html$/,
            exclude: /node_modules/,
            loader: 'file-loader',
            options: { name: '[name].[ext]' }
        }, {
            test: /\.(sa|sc|c)ss$/,
            use: [
                {
                    loader: MiniCssExtractPlugin.loader,
                },
                'css-loader',
                'sass-loader',
            ]
        }, {
            test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
            use: [
                {
                    loader: 'url-loader',
                    options: {
                        limit: 10000,
                        mimetype: 'application/font-woff'
                    }
                }]
        }, {
            test: /(all|rts|lib|out|runmain).js$/,
            use: [{ loader: 'file-loader' }]
        }, {
            test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
            use: [{ loader: 'file-loader' }]
        }]
    },
    plugins: [
        new MiniCssExtractPlugin({
            filename: '[name].css',
            chunkFilename: '[id].css',
            ignoreOrder: false,
        })
    ]
};
