<!-- 

  Feuille de style très élémentaire pour transformer les fichiers xml des délibération en fichiers bruts.
  
  Les options sont limitées par le fait que les délibérations sont le plus souvent formatées dans le texte même (retours de chariot, tabulation, puces…) et non par des balises.
  
  Cette feuille consiste donc principalement à rajouter des sauts à la ligne (dans la plus part des cas), des puces (pour les listes) ou à ne rien faire d'autre que supprimer les balises.
  
-->
<xsl:stylesheet   
  version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
>
  <xsl:output method="text" indent="no" encoding="UTF-8" omit-xml-declaration="yes"/>
  
  <!-- ROOT -->
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
      <!-- </p><ul>|</p><ol>  -->
      <xsl:when test="following-sibling::node()[1][self::ul] or following-sibling::node()[1][self::ol]">
        <!-- <xsl:text disable-output-escaping="yes">par-l:</xsl:text> -->
      </xsl:when>
      <xsl:otherwise>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <!-- listes -->
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
  
  <!-- mise à plat des tables -->
  <xsl:template match="table">
    <xsl:text disable-output-escaping="yes">&#10;</xsl:text>
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="tr">
    <!-- <xsl:text disable-output-escaping="yes">&#10;</xsl:text> -->
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="tbody|thead">
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="td|th">
    <xsl:text disable-output-escaping="yes">&#10;</xsl:text>
    <xsl:apply-templates />
  </xsl:template>
  
  <!-- 
  ¡¡¡FIXME!!! 
  
  Dans le cas suivant (féquent)
  
  `Lorem ipsum dolor sit amet, consectetur adipiscing elit.<br/>
  <br/>
  Sed non risus.`
  
  on obtient
  
  `Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  
  
  
  Sed non risus.`
  
  -->
  <xsl:template match="br">   
    <xsl:choose>
      <xsl:when test="not(ancestor::tr)">
        <xsl:text disable-output-escaping="yes"> &#10;</xsl:text> 
      </xsl:when>
      <xsl:otherwise>
        <!-- pas de saut à la ligne dans les tableaux -->
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <!-- -->
  <xsl:template match="div">
    <xsl:apply-templates />
  </xsl:template>
  
  <!-- -->
  <xsl:template match="font|strong|b|em|i|sup|u|tt">
    <xsl:apply-templates />
  </xsl:template>
  
  <!-- no match -->
  <xsl:template match="*">
    <!-- copie la balise et applique les templates aux nœuds inférieurs -->
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>
