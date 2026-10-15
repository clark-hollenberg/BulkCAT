SELECT 
    egt.g_rank,
    egt.ROUNDED_G_RANK rnd_g_rank,
    est.s_rank,
    sn.scientific_name "SName",
    est.element_subnational_id subntl_id,
    egt.elcode_bcd elcode,
    d_name_category.name_category_desc "MajorGroup",

    /* Rank Factors */

    /* EOO */
    d_range_extent.RANGE_EXTENT_CD "eoo_user",
    d_range_extent.RANGE_EXTENT_DESC "eoo_user_desc",

    /* AOO */

    D_AOO_GRID_SIZE.AOO_GRID_SIZE_DESC "aoo_grid_size",
    D_RANKING_SPATIAL_PATTERN.RANKING_SPATIAL_PATTERN_DESC "aoo_patch_type",

    CASE
        WHEN esr.D_AOO_4KM_ID IS NOT NULL
            THEN D_AOO_4KM.AOO_4KM_CD

        WHEN esr.D_AOO_1KM_ID IS NOT NULL
            THEN D_AOO_1KM.AOO_1KM_CD

        WHEN esr.D_AOO_DIRECT_MATRIX_ID IS NOT NULL
            THEN D_AOO_DIRECT_MATRIX.AOO_DIRECT_MATRIX_CD

        WHEN esr.D_AOO_DIRECT_DEFAULT_ID IS NOT NULL
            THEN D_AOO_DIRECT_DEFAULT.AOO_DIRECT_DEFAULT_CD

        WHEN esr.D_AOO_DIRECT_SMALL_ID IS NOT NULL
            THEN D_AOO_DIRECT_SMALL.AOO_DIRECT_SMALL_CD
    END "aoo_user",

    CASE
        WHEN esr.D_AOO_4KM_ID IS NOT NULL
            THEN D_AOO_4KM.AOO_4KM_DESC

        WHEN esr.D_AOO_1KM_ID IS NOT NULL
            THEN D_AOO_1KM.AOO_1KM_DESC

        WHEN esr.D_AOO_DIRECT_MATRIX_ID IS NOT NULL
            THEN D_AOO_DIRECT_MATRIX.AOO_DIRECT_MATRIX_DESC

        WHEN esr.D_AOO_DIRECT_DEFAULT_ID IS NOT NULL
            THEN D_AOO_DIRECT_DEFAULT.AOO_DIRECT_DEFAULT_DESC

        WHEN esr.D_AOO_DIRECT_SMALL_ID IS NOT NULL
            THEN D_AOO_DIRECT_SMALL.AOO_DIRECT_SMALL_DESC
    END "aoo_user_desc",

    /* Number of EOs */
    d_number_eos.NUMBER_EOS_CD "num_eo_user",
    d_number_eos.NUMBER_EOS_DESC "num_eo_user_desc",

    /* Population size */
    CASE 
        WHEN d_name_category.name_type_cd IN ('P', 'A')
        THEN d_pop_size.POP_SIZE_CD
        ELSE ''
    END "pop_size",

    /* Number of good EOs */
    d_number_good_eos.NUMBER_GOOD_EOS_CD "num_good_eo",
    d_number_good_eos.NUMBER_GOOD_EOS_DESC "num_good_eo_user_desc",

    /* Proportion of area good */
    NULL "perc_good_area",
    
    /* Short-term trend */
    d_short_term_trend.SHORT_TERM_TREND_CD "trend_short",

    /* Long-term trend */
    d_long_term_trend.LONG_TERM_TREND_CD "trend_long",

    /* Threat */
    D_THREAT_IMPACT_ASSIGNED.THREAT_IMPACT_ASSIGNED_CD "threat",

    /* Intrinsic vulnerability */
    d_intrin_vulnerability.INTRIN_VULNERABILITY_CD "vulnerability",

    /* Environmental specificity */
    d_enviro_specificity.ENVIRO_SPECIFICITY_CD "enviro_specificity",


    /* Comments Fields */
    NULL Adjust_Reasons,
    esr.S_RANK_REASONS, 
    esr.VERSION_AUTHOR,
    esr.VERSION_DATE,
    esr.INTERNAL_NOTES,
    esr.S_RANGE_COM,
    esr.S_AREA_DIST_OF_OCCUPANCY_COM,
    esr.S_NUMBER_EOS_COM,
    esr.S_POP_SIZE_COM,
    esr.S_NUMBER_GOOD_EOS_COM,
    esr.S_ENVIRO_SPECIFICITY_COM, 
    esr.S_SHORT_TERM_TREND_COM, 
    esr.S_LONG_TERM_TREND_COM,
    esr.S_THREAT_COM,
    esr.S_INTRINSIC_VULNERABILITY_COM

FROM
    element_global egt,
    element_national ent,
    element_subnational est,
    element_subnatl_rank esr,
    scientific_name sn,
    subnation,
    d_classification_level,
    d_name_category,
    d_number_eos, 
    d_number_good_eos,
    d_number_prot_eos,
    d_pop_size,
    d_range_extent,
    D_AOO_4KM,
    D_AOO_1KM,
    D_AOO_GRID_SIZE,
    D_RANKING_SPATIAL_PATTERN,
    D_AOO_DIRECT_MATRIX,
    D_AOO_DIRECT_DEFAULT,
    D_AOO_DIRECT_SMALL,
    d_long_term_trend, 
    d_short_term_trend,
    d_threat_impact_assigned,
    d_intrin_vulnerability,
    d_enviro_specificity

WHERE 
    egt.element_global_id = ent.element_global_id
    and ent.element_national_id = est.element_national_id
    and est.element_subnational_id = esr.element_subnational_id
    and est.sname_id = sn.scientific_name_id
    and est.subnation_id = subnation.subnation_id
    and sn.d_classification_level_id = d_classification_level.d_classification_level_id
    and sn.d_name_category_id = d_name_category.d_name_category_id
    and esr.d_number_eos_id = d_number_eos.d_number_eos_id (+)
    and esr.d_number_good_eos_id = d_number_good_eos.d_number_good_eos_id (+)
    and esr.d_number_prot_eos_id = d_number_prot_eos.d_number_prot_eos_id (+)
    and esr.d_pop_size_id = d_pop_size.d_pop_size_id (+)
    and esr.d_range_extent_id = d_range_extent.d_range_extent_id (+)
    and esr.D_AOO_GRID_SIZE_ID = D_AOO_GRID_SIZE.D_AOO_GRID_SIZE_ID (+)
    AND esr.D_RANKING_SPATIAL_PATTERN_ID =
        D_RANKING_SPATIAL_PATTERN.D_RANKING_SPATIAL_PATTERN_ID (+)

    AND esr.D_AOO_4KM_ID =
        D_AOO_4KM.D_AOO_4KM_ID (+)

    AND esr.D_AOO_1KM_ID =
        D_AOO_1KM.D_AOO_1KM_ID (+)

    AND esr.D_AOO_DIRECT_MATRIX_ID =
        D_AOO_DIRECT_MATRIX.D_AOO_DIRECT_MATRIX_ID (+)

    AND esr.D_AOO_DIRECT_DEFAULT_ID =
        D_AOO_DIRECT_DEFAULT.D_AOO_DIRECT_DEFAULT_ID (+)

    AND esr.D_AOO_DIRECT_SMALL_ID =
        D_AOO_DIRECT_SMALL.D_AOO_DIRECT_SMALL_ID (+)
    and esr.d_long_term_trend_id = d_long_term_trend.d_long_term_trend_id (+)
    and esr.d_short_term_trend_id = d_short_term_trend.d_short_term_trend_id (+)
    and esr.D_THREAT_IMPACT_ASSIGNED_ID  = D_THREAT_IMPACT_ASSIGNED.D_THREAT_IMPACT_ASSIGNED_ID  (+)
    and esr.d_intrin_vulnerability_id = d_intrin_vulnerability.d_intrin_vulnerability_id (+)
    and esr.d_enviro_specificity_id = d_enviro_specificity.d_enviro_specificity_id (+)
and egt.inactive_ind = 'N' /* EXCLUDE INACTIVES */
and d_name_category.name_category_desc = 'International Vegetation Classification' /* EXCLUDE SPECIES */


ORDER BY  
d_name_category.name_category_desc,
sn.scientific_name
