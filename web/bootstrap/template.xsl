<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="html" encoding="UTF-8" omit-xml-declaration="yes" indent="yes"/>

<xsl:template match="/page">
  <xsl:param name="active-section" select="section/text()"/>

  <xsl:text disable-output-escaping="yes">&lt;</xsl:text>!DOCTYPE html<xsl:text disable-output-escaping="yes">&gt;</xsl:text>
  <html>
    <head>
      <meta charset="utf-8" />
      <title><xsl:value-of select="title"/></title>
      <link rel="stylesheet" type="text/css" href="bootstrap/css/bootstrap.css" />
      <style>
	body {
          padding-top: 60px; /* to make the container go all the way to the bottom of the topbar */
	}
      </style>
      <link rel="stylesheet" type="text/css" href="ui.css" />
      <script type="text/javascript" src="jquery-1.7.2.min.js"></script>
      <script type="text/javascript" src="jquery.stream-1.2.js"></script>
      <script type="text/javascript" src="ocamlmsg.js"></script>
      <script type="text/javascript" src="global.js"></script>
      <xsl:for-each select="load">
	<script type="text/javascript" src="{text()}"></script>
      </xsl:for-each>
      <xsl:comment>[if lt IE 9]<xsl:text disable-output-escaping="yes">&gt;
	  &lt;script src="http://html5shim.googlecode.com/svn/trunk/html5.js"&gt;&lt;/script&gt;
	  &lt;!</xsl:text>[endif]
      </xsl:comment>
    </head>
    <body>
      <div class="navbar navbar-fixed-top">
	<div class="navbar-inner">
	  <div class="container">
	    <a class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
	      <span class="icon-bar"></span>
	      <span class="icon-bar"></span>
	      <span class="icon-bar"></span>
	    </a>
	    <a class="brand" href="http://github.com/tonyg/ocamlmsg/">Ocamlmsg</a>
	    <div class="nav-collapse">
	      <ul class="nav">
		<li class="btn-danger"><a href="about.html">About</a></li>
		<xsl:for-each select="document('nav.xml')/navitems/navitem">
		  <li>
		    <xsl:if test="@id = $active-section">
		      <xsl:attribute name="class">active</xsl:attribute>
		    </xsl:if>
		    <a href="{@href}"><xsl:value-of select="text()"/></a>
		  </li>
		</xsl:for-each>
	      </ul>
	      <ul class="nav pull-right">
		<li><a id="server_status_message_container"><span id="server_status_message">Connected</span></a></li>
	      </ul>
	    </div>
	  </div>
	</div>
      </div>

      <div class="container">
	<xsl:copy-of select="body" />
      </div>

      <xsl:copy-of select="script" />

      <script src="bootstrap/js/bootstrap-transition.js"></script>
      <script src="bootstrap/js/bootstrap-alert.js"></script>
      <script src="bootstrap/js/bootstrap-modal.js"></script>
      <script src="bootstrap/js/bootstrap-dropdown.js"></script>
      <script src="bootstrap/js/bootstrap-scrollspy.js"></script>
      <script src="bootstrap/js/bootstrap-tab.js"></script>
      <script src="bootstrap/js/bootstrap-tooltip.js"></script>
      <script src="bootstrap/js/bootstrap-popover.js"></script>
      <script src="bootstrap/js/bootstrap-button.js"></script>
      <script src="bootstrap/js/bootstrap-collapse.js"></script>
      <script src="bootstrap/js/bootstrap-carousel.js"></script>
      <script src="bootstrap/js/bootstrap-typeahead.js"></script>
    </body>
  </html>
</xsl:template>

</xsl:stylesheet>
