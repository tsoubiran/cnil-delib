<!-- 

-->
<xsl:stylesheet   
  version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
>
  <xsl:output indent="no" encoding="UTF-8" omit-xml-declaration="yes"/>
  
  <xsl:template match="BLOC_TEXTUEL|CONTENU">
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="h1|h2">
    <xsl:text disable-output-escaping="yes">&#10;</xsl:text>
    <xsl:apply-templates />
    <xsl:text disable-output-escaping="yes">&#10;</xsl:text>
  </xsl:template>
  
  <xsl:template match="p">
    <xsl:choose>
      <xsl:when test="ancestor::table">
      </xsl:when>
      <xsl:otherwise>
        <xsl:text disable-output-escaping="yes">&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates />
    <xsl:choose>
      <xsl:when test="following-sibling::node()[1][self::ul] or following-sibling::node()[1][self::ol]">
        <xsl:text disable-output-escaping="yes">par-l:</xsl:text>
      </xsl:when>
      <xsl:otherwise>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="div">
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="br">   
    <xsl:choose>
      <xsl:when test="not(ancestor::tr)">
        <xsl:text disable-output-escaping="yes"> &#10;</xsl:text> 
      </xsl:when>
      <xsl:otherwise>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="font|strong|b|em|i|sup|u|tt">
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="ul|ol">
    <xsl:text disable-output-escaping="yes">&#10;</xsl:text>
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="li[ancestor::ul]">
    <xsl:text disable-output-escaping="yes">• </xsl:text>
    <xsl:apply-templates />
    <xsl:text disable-output-escaping="yes">&#10;</xsl:text>
  </xsl:template>
  
  <xsl:template match="li[ancestor::ol]">
    <xsl:text disable-output-escaping="yes">• </xsl:text><xsl:apply-templates />
    <xsl:apply-templates />
    <xsl:text disable-output-escaping="yes">&#10;</xsl:text>
  </xsl:template>
  
  <xsl:template match="table">
    <xsl:text disable-output-escaping="yes">&#10;&#10;</xsl:text>
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="tr">
    <xsl:text disable-output-escaping="yes">&#10;</xsl:text>
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="tbody|thead">
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="td|th">
    <xsl:text disable-output-escaping="yes">&#10;</xsl:text>
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="*">
    <xsl:value-of select="name()"/><xsl:text disable-output-escaping="yes">:(</xsl:text>
    <xsl:apply-templates />
   <xsl:text disable-output-escaping="yes">):</xsl:text><xsl:value-of select="name()"/>
  </xsl:template>
  
</xsl:stylesheet>
