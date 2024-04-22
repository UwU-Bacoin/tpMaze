# For use in Si2 labs on Ubuntu computers
#
# Pretty-prints sbt scalastyle XML output to screen or to html file
#
# Thomas Anberrée, 2022-23

# Source
SCALASTYLE_RESULT_XML=target/scalastyle-result.xml

# Targets
TEMP=.temp
HTML_TEMP_FILE_NAME=scalastyle-result-temp.html
XML_TEMP_FILE_NAME=index.xml
XML_TEMP_FILE_PATH=$TEMP/$XML_TEMP_FILE_NAME
HTML_TEMP_FILE_PATH=$TEMP/$HTML_TEMP_FILE_NAME
XSL_FILE_NAME=scalastyle-style.xsl
PYTHON_SERVER_FILE_NAME=server.py
PORT=8015
HLINE="-----------------------------------------------------------------------------"

mkdir -p .temp

cat >$TEMP/$XSL_FILE_NAME <<-EOM
<?xml version="1.0"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <html>
    <head>
    <title>Rapport Scalastyle</title>
	<style>
		.error    {color: red;}
		.warning    {color: yellow;}
		h2 {color: lime;}
		body {background-color: black; color: white; font-family: sans-serif;}
	</style>
    </head>
    <body>
    <h2>Rapport Scalastyle</h2>
    <table border="1" cellspacing="0" cellpadding="2">
	<tr>
	    <th colspan="2"><b>Résumé</b></th>
	</tr>
	<tr>
	    <td>Fichiers contenant des erreurs</td>
	    <td><xsl:number level="any" value="count(descendant::file[error])"/></td>
	</tr>
	<tr>
	    <td>Total errors</td>
	    <td><xsl:number level="any" value="count(descendant::error)"/></td>
	</tr>
	<tr>
	    <td>Erreurs par fichiers</td>
	    <td><xsl:number level="any" value="count(descendant::error) div count(descendant::file)"/></td>
	</tr>
    </table>



	<xsl:apply-templates/>

    </body>
  </html>
</xsl:template>

<xsl:template match="file[error]">
    <h2>

		<xsl:value-of select="concat('./src/main/',substring-after( @name , '/src/main/'))"/>
    </h2>
    <table width="95%" border="1" cellspacing="0" cellpadding="2">
	<tr>
	    <th> Ligne n° </th>
		<th> Sévérité </th>
	    <th> Message </th>
	</tr>
	<xsl:apply-templates select="error">
		<xsl:sort select="@severity" />
		<xsl:sort select="@line" />
	</xsl:apply-templates>
    </table>
    <p/>
</xsl:template>

<xsl:template match="error">
    <tr>
	<td>
	    <xsl:value-of select="@line"/>
	</td>
	<td><span class="{@severity}">
	    <xsl:value-of select="@severity"/>
	</span></td>
	<td>
	    <xsl:value-of select="@message"/>
	</td>
    </tr>
</xsl:template>

</xsl:stylesheet>
EOM

cat >$TEMP/$PYTHON_SERVER_FILE_NAME <<-EOM
# server.py
import http.server  # Our http server handler for http requests
import socketserver  # Establish the TCP Socket connections

PORT = $PORT
DIR = '$TEMP/'


class MyHttpRequestHandler(http.server.SimpleHTTPRequestHandler):
    def do_GET(self):
        if self.path == '/':
            self.path = DIR + 'index.xml'
        else:
            self.path = DIR + self.path

        return http.server.SimpleHTTPRequestHandler.do_GET(self)


Handler = MyHttpRequestHandler

with socketserver.TCPServer(("", PORT), Handler) as httpd:
    print("Http Server Serving at port", PORT)
    httpd.serve_forever()

EOM

if ! command -v sbt &>/dev/null; then
    echo $HLINE
    echo "The sbt command could not be found. See instructions there :"
    echo "        https://www.scala-sbt.org/download.html"
    echo $HLINE
else
    if [ $# -eq 0 ]; then
        sbt scalastyle
    fi

    cat $SCALASTYLE_RESULT_XML >$XML_TEMP_FILE_PATH

    # adds a reference to the XLS template in the XML file
    sed -i '1 s|$|<?xml-stylesheet type="text/xsl" href="'$XSL_FILE_NAME'" ?>|' $XML_TEMP_FILE_PATH

    pretty_printed=0
    if ! command -v xsltproc &>/dev/null; then
        echo $HLINE
        echo "The xsltproc command could not be found. You may install it with : "
        echo "        sudo apt install xsltproc"
        echo $HLINE

    else
        # creates html file from XML and XSL files
        xsltproc $XML_TEMP_FILE_PATH -o $HTML_TEMP_FILE_PATH

        if ! command -v elinks &>/dev/null; then
            echo $HLINE
            echo "The elinks command could not be found. You may install it with : "
            echo "        sudo apt install elinks"
            echo $HLINE
        else
            clear
            elinks -dump -dump-color-mode 1 $HTML_TEMP_FILE_PATH
            pretty_printed=1

        fi

    fi

    if [ $pretty_printed -eq 0 ]; then
        if lsof -Pi :$PORT -sTCP:LISTEN -t >/dev/null; then
            echo "Reload in your browser or ctrl+click this link http://0.0.0.0:$PORT ."
        else
            echo "To view report in Firefox, ctrl+click this link : http://0.0.0.0:$PORT ."
            python3 $TEMP/$PYTHON_SERVER_FILE_NAME >/dev/null 2>&1 &
        fi
        echo ""
        echo "(If you need to kill the server : fuser -k $PORT/tcp)"
    fi
fi
