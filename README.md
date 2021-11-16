---
author:
- M.Weynants
bibliography:
- ./Biblio/swat.bib
- ./Biblio/R.bib
- ./Biblio/PTF/ptf.bib
- ./Biblio/esdb.bib
title: sgdbe2swat: SWAT layer from SGDBE
---
# sgdbe2swat
Preparation of SWAT.SOL input file based on the Soil Geographical Database of Eurasia (SGDBE).

! WARNING ! Files date back from March 2012.
They are valid for SWAT2009. No check has been made on whether input files for later versions of SWAT require modifications.


Introduction
============

Soil Water Assessment Tool (SWAT) is a comprehensive hydrological model
working at the river basin scale. It was developed to quantify the
impact of land management practices in large, complex watersheds
(<http://swatmodel.tamu.edu/>). Among all the information required to
run the model, parameters relative to the soil physical, chemical and
mechanical properties are needed.\
The Soil Geographical Database of Eurasia (SGDBE) provides a harmonised
set of soil parameters covering Eurasia and Mediterranean countries at
scale 1:1,000,000 [@EC2003]. It is part of the European Soil Database
(ESDB), along with the Pedotransfer Rules Database (PTRDB), the Soil
Profile analytical Database (SPADE) and the Database of Hydrological
Properties of European Soils [HYPRES @Wosten1999]. Information in SGDBE
is available at the Soil Typological Unit (STU) level, characterised by
attributes specifying the nature and properties of soils. For mapping
purposes, the STUs are grouped into Soil Mapping Units (SMU) since it is
not possible to delineate each STU at the 1:1,000,000 scale [@EC2003].\
Newer hydraulic pedotransfer functions [@Toth2014] were developed on the
European Hydropedological Inventory [EU-HYDI @Weynants2013] with a
better coverage for soils of Eastern and Southern Europe. In the text,
they are further referred to as euptf.

Thanks to the availability of the ESDB and the new PTFs, we propose to
generate the SWAT.SOL input file for all SMUs. The soil properties are
first gathered at the STU level and are then delivered at the SMU level,
based on the dominant STU.

Generation of SWAT.SOL inputs {#swat}
=============================

Information at the STU level is categorical. SWAT.SOL requires
continuous input data. SWAT.SOL input data and their definition are
given in Table [1](#tab:SWATinput){reference-type="ref"
reference="tab:SWATinput"}. The estimation method of the SWAT.SOL input
data at the STU level is summarised in Table
[2](#tab:esdb2swat){reference-type="ref" reference="tab:esdb2swat"}. An
extended description is given in the text.\
The attributes needed at the soil layer level could either be retrieved
from the estimated profile database or from the pedotransfer rules
(PTR). The former gives a more detailed account of a representative
profile for the STU, but it is not available for all STUs. The latter
gives rough categorical estimations for the topsoil and the subsoil of
each STU. In order to provide homogeneous data, only attributes derived
from the STU properties or from PTR are used.\
All data were processed in R (version version 3.0.1). The main script
([esdb2swat.R]{.sans-serif}) and all the functions created to generate
the SWAT inputs are distributed with this document.

::: {#tab:SWATinput}
  FIELD        RANGE         DEFINITION
  ------------ ------------- -----------------------------------------------------------------------------------------------
  SNAM                       Soil name
  HYDGRP       \[A,B,C,D\]   Soil Hydrologic Group
  SOL_ZMX      \[0,3500\]    Maximum rooting depth of soil profile (mm).
  ANION_EXCL   \[0.01,1\]    Fraction of porosity (void space) from which anions are excluded. \[OPTIONAL\] (default: 0.5)
  SOL_CRK      \[0,1\]       Crack volume potential of soil (% soil volume). \[OPTIONAL\]
  TEXTURE                    Texture classes of soil layers. \[OPTIONAL\]
  NLAYERS      \[1,10\]      Number of layers in the soil.
  SOL_Z*i*     \[0,3500\]    Depth from soil surface to bottom of layer (mm).
  SOL_BD*i*    \[0.9,2.5\]   Moist bulk density (g/cm$^3$).
  SOL_AWC*i*   \[0,1\]       Available water capacity of the soil layer (mm water/mm soil).
  SOL_K*i*     \[0,2000\]    Saturated hydraulic conductivity (mm/hr).
  SOL_CBN*i*   \[0.05,10\]   Organic carbon content (% soil weight).
  CLAY*i*      \[0,100\]     Clay ($<$0.002 mm) content (% soil weight).
  SILT*i*      \[0,100\]     Silt (between 0.002 and 0.05 mm) content (% soil weight).
  SAND*i*      \[0,100\]     Sand (between 0.05 and 2 mm) content (% soil weight).
  ROCK*i*      \[0,100\]     Rock fragment ($>$ 2 mm) content (% total weight).
  SOL_ALB1     \[0,0.25\]    Moist soil albedo (only for surface layer ($i=1$)).
  USLE_K1      \[0,0.65\]    USLE equation soil erodibility (K) factor (only for surface layer ($i=1$)).
  SOL_EC*i*    \[0,100\]     Electrical conductivity (dS/mm). \[Not currently active\]

  : Codes of SWAT.SOL input attributes, with their accepted range and
  their description. ($i \in [1,10]$)
:::

::: {#tab:esdb2swat}
  FIELD        R FILE      DESCRIPTION
  ------------ ----------- ----------------------------------------------------------------------------------------------------
  SNAM                     STU/SMU code
  HYDGRP       hydrgrp.R   PTR based on topsoil and subsoil dominant texture classes and depth class of an impermeable layer.
  SOL_ZMX      solzmx.R    Recode depth class of an obstacle to roots (ROO)
  ANION_EXCL               Set to default value: 0.5
  SOL_CRK                  Set to default value: 0
  TEXTURE                  Layers texture codes
  NLAYERS      solzmx.R    Default to 2.
  SOL_Z*i*     solzmx.R    First layer set to 30 cm, second to SOL_ZMX
  SOL_BD*i*                Obtained from euptf
  SOL_AWC*i*               Obtained from euptf
  SOL_K*i*                 Obtained from euptf
  SOL_CBN*i*   OC_sub.R    According to OC_TOP, set to 0, 1.5, 4 or 10 % weight. For subsoil, based on @Hiederer2009.
  CLAY*i*      av.text.R   Transform texture class (TEXTSRFDOM and TEXTSUBDOM) into clay, silt, sand contents.
  SILT*i*      av.text.R   
  SAND*i*      av.text.R   
  ROCK*i*                  According to PTR VS (volume of stone)
  SOL_ALB1     albedo.R    Based on [SWATxxx.mdb]{.sans-serif}
  USLE_K1                  According to PTR ERODIBILITY and @rusle2
  SOL_EC*i*                Set to default value: 0.

  : Rules for attributing SWAT.SOL input values to STU's.
:::

SNAM
----

The soil name is set to the STU code when building the file. In the
final product it is set to the SMU code.

HYDGRP
------

The Soil Hydrologic Group (HYDGRP) classes are defined according to the
National Engineering Handbook [@USDA2007], on the basis of the depth
class to an impermeable layer and the saturated hydraulic conductivity
of the least transmissive layer. The former is directly available for
each STU. The latter is obtained from the dominant texture class of the
surface and subsurface layers using euptf [@Toth2014]. A new rule is
therefore established. When no information is available for one or two
of the input variables, the least favourable class applicable to the
other variable(s) is attributed. When no information at all is
available, hydrologic group cannot be set. For non-soil STUs, HYDGRP is
set to class D.

SOL_ZMX
-------

The maximum rooting depth is obtained using STU attribute ROO, which
gives the depth class of an obstacle to roots. Other attributes may
restrict the depth: IL, presence of an impermeable layer, WR,
waterlogging, AGLIM1, dominant limitation to agricultural use and DR,
depth to rock, obtained with a PTR. All variables give depth classes.
The average value of the interval defined for each class is used.
SOL_ZMX is set to the most restictive these conditions.

ANION_EXCL
----------

The proportion of the soil porosity from which anions are excluded is
set to the SWAT default value of 0.5. Litterature has to be investigated
further to derive a PTR.

SOL_CRK
-------

The potential volume of crack of the soil profile is set to SWAT default
value of 0.5. Later versions could include the application of a PTR for
clayey soils susceptible to swelling/shrinking, using the World Base
Reference soil class to identify Vertisols (WRB-GRP VR) or mineralogy
(type of clay) as an argument.

TEXTURE
-------

This value is not processed, but it gives a summary of the layers
texture. We used the same texture classes as in SGDBE (C: Coarse; M:
Medium; MF: Medium Fine; F: Fine; VF: Very Fine; O: Organic; ?: No
information), separated by a dash in cases where there are two layers.

NLAYERS
-------

The number of layers defaults to 2, unless SOL_ZMX is less or equal to
30 cm.\
The subsequent attributes must be set for each layer.

SOL_Zi {#solz}
------

The depth of the first layer, SOL_Z1, defaults to 30 cm, unless SOL_ZMX
is less than 30 cm. If present, the depth of the second layer, SOL_Z2,
is equal to SOL_ZMX.

SOL_BD
------

The layer bulk density (BD) is obtained from the saturated water content
applying $$2.65 (1 - \theta_s)$$ where 2.65 g.cm$^{-3}$ is the density
of the soil mineral and ($\theta_s$) is the saturated water content
resulting from euptf with texture class and layer depth
(topsoil/subsoil) as inputs.

SOL_CBN
-------

The organic carbon (OC) content of the topsoil can be approximated using
PTR21's result OC_TOP. The possible values Very low, Low, Medium and
High are transformed into 0.1, 1.5, 4 and 10 % soil weight respectively.
PTR 21 has been revised by @Jones2005. In the revised version, there are
6 classes of OC_TOP, allowing to better estimate soil organic carbon
content of organic soils. However the output of the revised PTR is only
available as a raster file, because it uses CLC99 as land use input and
applies temperature corrections on the mapped result of the PTR. The use
of the revised PTR is possible but requires the genereation of new
SMUs.\
We highlight here the issue of the handling of peat soils or organic
layers in SWAT, since, according to SWAT example database, the model's
maximum allowed value for organic carbon content is 10 % weight.\
For the subsoil, estimations were made based on @Hiederer2009. For
organic soils, OC content in the subsoil is set to 110 % of the value in
the topsoil, regardless of the land use. For mineral soils, different
coefficients are applied as a function of the land use. For arable
lands, OC content of the subsoil is set to 70 % of the value of the
topsoil, and for other land covers to 30 %.

SOL_CLAY, SOL_SILT and SOL_SAND
-------------------------------

The texture fractions are evaluated from the dominant surface and
subsurface textural classes. When these are unknown, PTR1 is used to
estimate the dominant surface textural class. The value assigned for
each class is the geometrical centre of the surface covered by the class
in the textural triangle. For soil layers with no information or with no
mineral texture, the sand, silt and clay contents are set to 0.

SOL_ROCK
--------

The proportion of rock fragments is obtained from PTR412 giving the
volume of stones (VS). The same value is used for both the surface and
the subsurface layers.

AWC and SOL_K
-------------

The layer available water content, AWC, defined as
$\theta_{{FC}} - \theta_{{WP}}$, with FC at pF 2.5 and WP at pF 4.2, and
SOL_K, the layer saturated conductivity, are obtained from HYPRES class
PTFs, with the texture class and the layer depth (topsoil/subsoil) as
input.

SOL_ALB
-------

The soil albedo is roughly estimated from the soil carbon content, with
a PTF calibrated on the sample soil dataset provided with SWAT (tables
[usersoil]{.sans-serif} in [SWAT2009.mdb]{.sans-serif}). The minimum
albedo is set to 0.01 and the maximum to 0.23, for soil organic carbon
content greater or equal to 1.45 % weight. Between these two extremes a
simple linear regression is applied.

USLE_K
------

The erodibility factor of the USLE equation is estimated from PTR623,
which gives classes of erodibility. Values are attributed to the classes
according to the RUSLE reference guide [@rusle2].

Deliverable
===========

Files are provided as inputs ready for SWAT2009. An ESRI grid, 1 km
resolution, gives the soil mapping units (SMU) for Eurasia. A ESRI
shapefile with the same information is also included. A dBase table
gives SWAT.SOL required data. The table must be appended to the
[usersoil]{.sans-serif} table in [SWATxxxx.mdb]{.sans-serif}. The SNAM
field gives the SMU code and is used to link the table and the grid. The
properties are those of the dominant STU. The representativeness of the
dominant STU in the SMU is variable. Figure
[1](#fig:histSTUdom){reference-type="ref" reference="fig:histSTUdom"}
shows a histogram of the percentage of the SMU area covered by the
dominant STU. The frequency of SMU for which the dominant STU cannot be
clearly identified are shown in a different colour.

![Frequency of percentage of SMU area covered by dominant STU. The flag
indicates whether there are more than on STU covering the same
proportion of the SMU area (TRUE) or there is only one
(FALSE)](../Rcode/hist.pdf){#fig:histSTUdom}

#### Warning!

The dBase file could not be imported to [SWAT2009.mdb]{.sans-serif} with
Microsoft Access 2010. A csv file was created and used instead. However
when imported and appended to [usersoil]{.sans-serif} table, some fields
failed to produce the expected values. SOL_AWC2 and SOL_K2 were
considered as integers while they are floats (and they are in the csv
and appear as such if open with Excel). However when ckecking the Field
Size in Design View, it is rightly set to Double and not Intger.
