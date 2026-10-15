SELECT
    sn.SCIENTIFIC_NAME AS SNAME,
    sfp.SOURCE_FEATURE_ID AS SF_ID,
    gv.x AS LONGITUDE,
    gv.y AS LATITUDE,
    eo.EO_ID,
    eo_rank.BASIC_EO_RANK_CD

FROM
    SOURCE_FEATURE_REP_PROJ sfp,
    SOURCE_FEATURE sf,
    ELEMENT_SUBNATIONAL est,
    SCIENTIFIC_NAME sn,
    EO eo,
    EO_SOURCE_FEATURE eo_sf,
    D_BASIC_EO_RANK eo_rank,

    TABLE(
        SDO_UTIL.GETVERTICES(
            MDSYS.SDO_CS.TRANSFORM(
                SDO_GEOM.SDO_CENTROID(sfp.SHAPE, 0.005),
                0.005,
                4326
            )
        )
    ) gv

WHERE

    -- Connect source feature to its scientific name
    sfp.SOURCE_FEATURE_ID = sf.SOURCE_FEATURE_ID

    AND sf.ELEMENT_SUBNATIONAL_ID = est.ELEMENT_SUBNATIONAL_ID

    AND est.SNAME_ID = sn.SCIENTIFIC_NAME_ID

    -- Connect source feature to EO, but allow no EO
    AND sfp.SOURCE_FEATURE_ID = eo_sf.SOURCE_FEATURE_ID(+)

    AND eo_sf.EO_ID = eo.EO_ID(+)

    -- Connect EO to rank table, but allow no rank
    AND eo.D_BASIC_EO_RANK_ID = eo_rank.D_BASIC_EO_RANK_ID(+)

    -- Keep only Vascular Plants
    AND GET_MAJORGROUP_WORD(est.ELEMENT_SUBNATIONAL_ID) = 'Vascular Plants'

    -- Remove X and X? ranked EOs
    AND sfp.SOURCE_FEATURE_ID NOT IN
    (
        SELECT eo_sf2.SOURCE_FEATURE_ID
        FROM EO eo2
        LEFT OUTER JOIN EO_SOURCE_FEATURE eo_sf2
            ON eo2.EO_ID = eo_sf2.EO_ID
        WHERE eo2.D_BASIC_EO_RANK_ID IN (17,20)
    )

    -- Remove ID-questionable EOs
    AND sfp.SOURCE_FEATURE_ID NOT IN
    (
        SELECT eo_sf3.SOURCE_FEATURE_ID
        FROM EO eo3
        LEFT OUTER JOIN EO_SOURCE_FEATURE eo_sf3
            ON eo3.EO_ID = eo_sf3.EO_ID
        WHERE eo3.D_ID_CONFIRMED_ID = 3
    )