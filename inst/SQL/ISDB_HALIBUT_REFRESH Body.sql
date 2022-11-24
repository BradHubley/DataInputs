create or replace PACKAGE BODY              ISDB_halibut_refresh AS
	cursor status is
		select * 
			from survey_edit_status
                        where year = 2008;
			/*where year = (select max(year) from survey_edit_status);*/
	edit	survey_edit_status%rowtype;

   	state vdc.mwmfdutil.state_data%rowtype;
	staff vdc.mwmfdutil.staff_data%rowtype;
	divn_abbr varchar2(15);

PROCEDURE make_00_ISD_LF
/*
PROCEDURE make_00_ISD_LF

Procedure Name:		make_00_ISD_LF
Author:				Pierre C. Brien
Date Written:		2003/02/04
Function:
   MAKE  VIEW ISD_LF (BOOTSTRAP version) The following procedures require that an object named ISD_LF exist
   this procedure creates a minimal temporary object with the proper key and data columns.

   The code table(view) issettypeabbrev  which defines set type abbreviations is also created by this procedure.

	This uses Dynamic Natural SQL to create the object named. For example
	the PROCEDURE make_00_ISD_LF creates the object ISD_LF

Modification History (as Date YYYY/MM/DD  initials - change description):
	2003/02/04		conversion from script to PLSQL package using DNS
	2003/04/15		final changes to implement ISD_ALLCAT and ISD_ALLLF
	2005/10/26	db	Make ISD_LF a snapshot, instead of a Table

*/
IS
	dbms_sql_feedback       INTEGER;
	success                 BOOLEAN := FALSE;
BEGIN						  
execute immediate 'DROP snapshot ISD_LF';

execute immediate
	'CREATE snapshot ISD_LF
		refresh on demand as
		SELECT
            t.Trip,
            f.Set_No,
       	    f.setcd_id,
       	    c.speccd_id,
       	    s.common species,
            c.Est_Num_Caught,
       	    isset.sample_num(c.catch_id) Sample_Num,
            a.SexCd_Id,
            a.species_Sample_weight  sample_wt,
       	    l.Fish_Length,
            l.Num_At_Length,
            f.FishSet_id,
            c.Catch_Id
         FROM
              observer_ISTrips t,
              observer_ISFishSets f,
              observer_ISCatches c,
       	   observer_ISsamples a,
       	   mflib_species_codes s,
              observer_ISFishLengths l
        WHERE s.research    = c.speccd_id
          AND t.Trip_Id     = f.Trip_Id
          AND f.FishSet_Id  = c.FishSet_Id
          AND c.Catch_Id    = a.Catch_Id
          AND a.Smpl_Id     = l.Smpl_Id
          AND NVL(f.HaulCCd_Id,1) IN (1,2,3)
          AND t.TripCd_Id   in (7057)
          AND t.Owner_Group in (''USER_HALIBUT'',''USER_JAVITECH'',''USER_ACD'')';

execute immediate 'grant select on ISD_LF to vdc, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

execute immediate
       'create or replace view issettypeabbrev as
        select  setcd_id,
                set_type,
                DECODE(setcd_id,1,''COMM'',2,''TLEN'',3,''TBYC'',4,''SVFX'',5,''SVRN'',
        		      6,''FCHO'',7,''CNPR'',8,''EXPL'',9,''EXPR'',10,''INDX'') abbrev
        from observer_issettypecodes ';

execute immediate 'grant select on issettypeabbrev to vdc, vdc_dev, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

END;


PROCEDURE make_01_ISD_INF
/*
Procedure Name:		make_01_ISD_INF
Author:				Pierre C. Brien
Date Written:		2003/02/04
Function:
   MAKE  TABLE ISD_INF

    This routine gathers data from several ISDB tables to create a mission set information
    table. Variables to note are the
        lat and long in several formats
        season - computed
        soaktime - in minutes
        lenLLkm - length of longline in km
        nK_hooks - thousands of hooks

    Once the basic set info is prepared
    we Correct the INF to account for split sets (one vessel) and Multiple vessels per station per year
    ISD_INF_S4  summed   setcd_id = 4 - sum split sets
    ISD_INF_A4  averaged setcd_id = 4 - average where more than one vessel per station per year

    then do a union of setcd_id != 4 with ISD_INF_A4 to produce ISD_INF2
    ISD_INF2 contains sets which were 3+ hours soak time and had at least 500 hooks

    ISD_INF_A7058 is a table ov average durations and nK_hooks for tripcd_id = 7058 trips.


Modification History (as Date YYYY/MM/DD  initials - change description):
	2003/02/04		conversion from script to PLSQL package using DNS
	2003/04/15		final changes to implement ISD_ALLCAT and ISD_ALLLF
	2005/10/26	db	Make ISD_INF a snapshot, instead of a Table

*/
IS
	dbms_sql_feedback       INTEGER;
	success                 BOOLEAN := FALSE;
BEGIN

begin	 
	execute immediate 'DROP snapshot ISD_INF';
exception when others then null;
end;

           --MMM 2017 added new gear specifications
           --MAINLINE TYPE 107
           --MAINLINE MATERIAL 108
           --MAINLINE DIAMETER 109
           --MAINLINE WEIGHTS (AVG_KG) 110
           --WEIGHTLINE LENGTH 111
execute immediate
    'CREATE snapshot ISD_INF refresh on demand
       AS SELECT
           SUBSTR(v.vessel_name,1,15) vessel, t.tripcd_id, TO_NUMBER(TO_CHAR(t.board_date,''YYYY'')) year,
          decode(TO_CHAR(t.Board_Date,''MM''),12,''WINTER'',1,''WINTER'',2,''WINTER'',3,''SPRING'',4,''SPRING'',5,''SPRING'',
                   6,''SUMMER'',7,''SUMMER'',8,''SUMMER'',9,''FALL'',10,''FALL'',11,''FALL'',''MISSING'') season,
           SUBSTR(TO_CHAR(t.board_date,''MON''),1,3) month, v.cfv, t.trip, f.set_no, f.nafarea_id,
           SUBSTR (isset.get_nafo_div(f.nafarea_id), 1, 2) nafdiv, f.stratum_id,
           to_number(f.station) station, NVL(f.haulccd_id,1) haulccd_id,
           p1.longitude*-1 p1long, p1.latitude p1lat, p2.longitude*-1 p2long, p2.latitude p2lat,
           p3.longitude*-1 p3long, p3.latitude p3lat, p4.longitude*-1 p4long, p4.latitude p4lat,
           NVL(p1.longitude, p2.longitude)*-1 slongitude, NVL(p1.latitude,p2.latitude) slatitude,
           NVL(p3.longitude, p4.longitude)*-1 elongitude, NVL(p3.latitude,p4.latitude) elatitude,
           p1.longddmm p1longddmm, p1.latddmm p1latddmm,p2.longddmm p2longddmm, p2.latddmm p2latddmm,
           p3.longddmm p3longddmm, p3.latddmm p3latddmm,p4.longddmm p4longddmm, p4.latddmm p4latddmm,
           isset.day_and_time (p1.setdate,p1.settime) SDayTime,
           isset.day_and_time (p2.setdate,p2.settime) P2DayTime,
           isset.day_and_time (p3.setdate,p3.settime) P3DayTime,
           isset.day_and_time (p4.setdate,p4.settime) EDayTime,
           p1.settime p1time, 
           p2.settime p2time,
           p3.settime p3time,
           p4.settime p4time,
           DECODE(NVL(p3.settime-p2.settime,-999999),-999999,NULL,1440*(isset.day_and_time (p3.setdate,p3.settime)-isset.day_and_time (p2.setdate,p2.settime))) soakminp3p2,
           DECODE(NVL(p3.settime-p1.settime,-999999),-999999,NULL,1440*(isset.day_and_time (p3.setdate,p3.settime)-isset.day_and_time (p1.setdate,p1.settime))) soakminp3p1,
           DECODE(NVL(p4.settime-p1.settime,-999999),-999999,NULL,
           1440*(isset.day_and_time (p4.setdate,p4.settime)-isset.day_and_time
           (p1.setdate,p1.settime))) duration,
           len_longline lenLLkm,
           Round((f.num_hook_haul/ 1000.), 2) nK_hooks,
           (f.num_hook_haul*gf105.feature_value) gear_len_m,
           bait.feature bait_type,
           gf103.feature_value gangion_length,
           gf107.feature_value mainline_type,
           gf108.feature_value mainline_mat,
           gf109.feature_value mainline_diam,
           gf110.feature_value mainline_wtavg_kg,
           gf111.feature_value mainline_len,
           gf160.feature_value bait_avg_wt,
           p1.depth p1depth, p2.depth p2depth, p3.depth p3depth, p4.depth p4depth,
           NVL(p1.depth, p2.depth) sdepth, NVL(p3.depth, p4.depth) edepth,
           g.gearcd_id, g.hookcd_id, g.hooksize, f.num_hook_haul,
           gf105.feature_value hookspacingm, f.specscd_id,
           t.vess_id, t.trip_id, f.fishset_id, f.setcd_id, a.set_type,
           p1.setprof_id p1setprof_id, p4.setprof_id p4setprof_id
      FROM
           observer_ISTrips t, observer_isvessels v,
           (select g.gear_id g_id, v.* from (select * from observer_isgearfeatures
           where gearfcd_id = 105) v,
           observer_isgears g where v.gear_id(+)=g.gear_id) gf105,
           (select g.gear_id g_id, v.* from (select * from observer_isgearfeatures
           where gearfcd_id = 107) v,
           observer_isgears g where v.gear_id(+)=g.gear_id) gf107,
           (select g.gear_id g_id, v.* from (select * from observer_isgearfeatures
           where gearfcd_id = 108) v,
           observer_isgears g where v.gear_id(+)=g.gear_id) gf108,
           (select g.gear_id g_id, v.* from (select * from observer_isgearfeatures
           where gearfcd_id = 109) v,
           observer_isgears g where v.gear_id(+)=g.gear_id) gf109,
           (select g.gear_id g_id, v.* from (select * from observer_isgearfeatures
           where gearfcd_id = 110) v,
           observer_isgears g where v.gear_id(+)=g.gear_id) gf110,
           (select g.gear_id g_id, v.* from (select * from observer_isgearfeatures
           where gearfcd_id = 111) v,
           observer_isgears g where v.gear_id(+)=g.gear_id) gf111,
           (select g.gear_id g_id, v.* from (select * from observer_isgearfeatures
           where gearfcd_id = 103) v,
           observer_isgears g where v.gear_id(+)=g.gear_id) gf103,
            (select g.gear_id g_id, v.* from (select * from observer_isgearfeatures
           where gearfcd_id = 160) v,
           observer_isgears g where v.gear_id(+)=g.gear_id) gf160,
           observer_isgears g,
           (select f.fishset_id f_id, p.* from (select * from observer_issetprofile where pntcd_id=1) p,
           observer_ISFishSets f where p.fishset_id(+)=f.fishset_id) p1,
           (select f.fishset_id f_id, p.* from (select * from observer_issetprofile where pntcd_id=2) p,
           observer_ISFishSets f where p.fishset_id(+)=f.fishset_id) p2,
           (select f.fishset_id f_id, p.* from (select * from observer_issetprofile where pntcd_id=3) p,
           observer_ISFishSets f where p.fishset_id(+)=f.fishset_id) p3,
           (select f.fishset_id f_id, p.* from (select * from observer_issetprofile where pntcd_id=4) p,
           observer_ISFishSets f where p.fishset_id(+)=f.fishset_id) p4,
           observer_ISFishSets f,
           issettypeabbrev a,
           (SELECT F1.gear_id,
        FC1.feature
      FROM isdb_halibut.observer_isgearfeatures F1,
        isdb_halibut.observer_isgearfeaturecodes FC1
      WHERE F1.gearfcd_id = FC1.gearfcd_id
      AND FC1.gearfcl_id  = ''BAIT TYPES'') bait
      WHERE
           f.fishset_id=p1.f_id AND f.fishset_id=p2.f_id
           AND f.fishset_id=p3.f_id AND f.fishset_id=p4.f_id
           AND t.vess_id=v.vess_id AND t.trip_id=f.trip_id AND g.gear_id=f.gear_id
           AND g.gear_id=gf105.g_id
           --new
           AND g.gear_id=gf107.g_id(+)
           AND g.gear_id=gf108.g_id(+)
           AND g.gear_id=gf109.g_id(+)
           AND g.gear_id=gf110.g_id(+)
           AND g.gear_id=gf111.g_id(+)
           AND g.gear_id=gf103.g_id(+)
           AND g.gear_id=gf160.g_id(+)
           AND g.gear_id=bait.gear_id(+)
           --end
           AND f.setcd_id    = a.setcd_id
           AND T.TripCd_Id in (7057,7058)
           AND T.owner_group in (''USER_HALIBUT'',''USER_JAVITECH'',''USER_ACD'')';

execute immediate 'grant select on ISD_INF to vdc, vdc_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';
/*
    Now Correct the INF to account for split sets (one vessel) and Multiple vessels per station per year
    ISD_INF_S4  summed   setcd_id = 4
    ISD_INF_A4  averaged setcd_id = 4
*/
begin	 
	execute immediate 'DROP snapshot ISD_INF_S4';
exception when others then null;
end;

execute immediate
    'CREATE snapshot ISD_INF_S4 refresh on demand AS
    (SELECT VESSEL, YEAR, STATION, STRATUM_ID,
        Count (*) countSets,
        MIN(TRIPCD_ID) TRIPCD_ID,   MIN(SEASON) SEASON, MIN(MONTH) MONTH, MIN(CFV) CFV, MIN(TRIP) TRIP,
        MIN(SET_NO) SET_NO, MIN(NAFAREA_ID) NAFAREA_ID, MIN(NAFDIV) NAFDIV, MIN(HAULCCD_ID) HAULCCD_ID,
        AVG(P1LONG) P1LONG, AVG(P1LAT     )   P1LAT, AVG(P2LONG    )   P2LONG, AVG(P2LAT     )   P2LAT,
        AVG(P3LONG    )   P3LONG, AVG(P3LAT     )   P3LAT, AVG(P4LONG    )   P4LONG, AVG(P4LAT     )   P4LAT,
        AVG(SLONGITUDE)   SLONGITUDE, AVG(SLATITUDE )   SLATITUDE,  AVG(ELONGITUDE)   ELONGITUDE,
        AVG(ELATITUDE )   ELATITUDE,
        MIN(P1LONGDDMM )  P1LONGDDMM , MIN(P1LATDDMM  )  P1LATDDMM  , MIN(P2LONGDDMM )  P2LONGDDMM ,
        MIN(P2LATDDMM  )  P2LATDDMM  , MIN(P3LONGDDMM )  P3LONGDDMM , MIN(P3LATDDMM  )  P3LATDDMM  ,
        MIN(P4LONGDDMM )  P4LONGDDMM , MIN(P4LATDDMM  )  P4LATDDMM  , MIN(SDAYTIME   )  SDAYTIME   ,
        MIN(P1TIME     )  P1TIME     , MIN(P2TIME     )  P2TIME     , MIN(EDAYTIME   )  EDAYTIME   ,
        MIN(P3TIME     )  P3TIME     ,  MIN(P4TIME     )  P4TIME    , MIN(SOAKMINP3P2)  SOAKMINP3P2,
        (SUM(DURATION *  NK_HOOKS)/SUM(NK_HOOKS)) DURATION,
        SUM(LENLLKM) LENLLKM, SUM(NK_HOOKS) NK_HOOKS, SUM(GEAR_LEN_M) GEAR_LEN_M,
        MIN(MAINLINE_TYPE) MAINLINE_TYPE, MIN(MAINLINE_MAT) MAINLINE_MAT, AVG(MAINLINE_DIAM) MAINLINE_DIAM, 
        AVG(MAINLINE_WTAVG_KG) MAINLINE_WTAVG_KG, AVG(MAINLINE_LEN) MAINLINE_LEN,
        AVG(P1DEPTH) P1DEPTH, AVG(P2DEPTH) P2DEPTH, AVG(P3DEPTH) P3DEPTH, AVG(P4DEPTH) P4DEPTH,
        AVG(SDEPTH) SDEPTH, AVG(EDEPTH) EDEPTH, MIN(GEARCD_ID) GEARCD_ID,MIN(HOOKCD_ID) HOOKCD_ID,
        MIN(HOOKSIZE) HOOKSIZE, SUM(NUM_HOOK_HAUL) NUM_HOOK_HAUL, AVG(HOOKSPACINGM) HOOKSPACINGM,
        MIN(SPECSCD_ID) SPECSCD_ID, Min(VESS_ID) VESS_ID, MIN(TRIP_ID) TRIP_ID, MIN(FISHSET_ID) FISHSET_ID,
        MIN(SETCD_ID) SETCD_ID, MIN(SET_TYPE) SET_TYPE, MIN(P1SETPROF_ID) P1SETPROF_ID,
        MIN(P4SETPROF_ID) P4SETPROF_ID
    FROM ISD_INF
    WHERE SET_TYPE = ''SURVEY - FIXED'' and Duration > 179
    GROUP BY VESSEL, YEAR, STATION, STRATUM_ID)';

execute immediate 'grant select on ISD_INF_S4 to vdc, vdc_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

/*
    ISD_INF_A4  averaged setcd_id = 4
*/
begin	 
	execute immediate 'DROP snapshot ISD_INF_A4';
exception when others then null;
end;

execute immediate
    'CREATE snapshot ISD_INF_A4 refresh on demand AS
    (SELECT  YEAR, STATION,  STATION  SET_NO, STRATUM_ID,  substr(YEAR||''-''||STATION,1,15) UNIQUE_ID,
        Count (*) countCFV, SUM(countSets) countSets, MIN (VESSEL) VESSEL,
        MIN(TRIPCD_ID) TRIPCD_ID,  MIN(CFV) CFV, MIN(TRIP) TRIP,
        MIN(NAFAREA_ID) NAFAREA_ID, MIN(NAFDIV) NAFDIV, MIN(HAULCCD_ID) HAULCCD_ID,
        AVG(P1LONG) P1LONG, AVG(P1LAT     )   P1LAT, AVG(P2LONG    )   P2LONG, AVG(P2LAT     )   P2LAT,
        AVG(P3LONG    )   P3LONG, AVG(P3LAT     )   P3LAT, AVG(P4LONG    )   P4LONG, AVG(P4LAT     )   P4LAT,
        AVG(SLONGITUDE)   SLONGITUDE, AVG(SLATITUDE )   SLATITUDE,  AVG(ELONGITUDE)   ELONGITUDE,
        AVG(ELATITUDE )   ELATITUDE,  MIN(SDAYTIME   )  SDAYTIME   ,
        MIN(P1TIME     )  P1TIME     , MIN(P2TIME     )  P2TIME     , MAX(EDAYTIME   )  EDAYTIME   ,
        MIN(P3TIME     )  P3TIME     ,  MIN(P4TIME     )  P4TIME    , MIN(SOAKMINP3P2)  SOAKMINP3P2,
        (SUM(DURATION *  NK_HOOKS)/SUM(NK_HOOKS)) DURATION,
        AVG(LENLLKM) LENLLKM, AVG(NK_HOOKS) NK_HOOKS, AVG(GEAR_LEN_M) GEAR_LEN_M,
        MIN(MAINLINE_TYPE) MAINLINE_TYPE, MIN(MAINLINE_MAT) MAINLINE_MAT, AVG(MAINLINE_DIAM) MAINLINE_DIAM, 
        AVG(MAINLINE_WTAVG_KG) MAINLINE_WTAVG_KG, AVG(MAINLINE_LEN) MAINLINE_LEN,
        AVG(P1DEPTH) P1DEPTH, AVG(P2DEPTH) P2DEPTH, AVG(P3DEPTH) P3DEPTH, AVG(P4DEPTH) P4DEPTH,
        AVG(SDEPTH) SDEPTH, AVG(EDEPTH) EDEPTH, MIN(GEARCD_ID) GEARCD_ID,MIN(HOOKCD_ID) HOOKCD_ID,
        MIN(HOOKSIZE) HOOKSIZE, AVG(NUM_HOOK_HAUL) NUM_HOOK_HAUL, AVG(HOOKSPACINGM) HOOKSPACINGM,
        MIN(SPECSCD_ID) SPECSCD_ID, Min(VESS_ID) VESS_ID, MIN(TRIP_ID) TRIP_ID, MIN(FISHSET_ID) FISHSET_ID,
        MIN(SETCD_ID) SETCD_ID, MIN(SET_TYPE) SET_TYPE
    FROM ISD_INF_S4
    WHERE SET_TYPE = ''SURVEY - FIXED'' and Duration > 179 AND NK_HOOKS > 0.500
    GROUP BY  YEAR, STATION, STATION, STRATUM_ID)';

execute immediate 'grant select on ISD_INF_A4 to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

execute immediate
    'CREATE OR REPLACE VIEW ISD_INF2  AS
    (SELECT YEAR, STATION, SET_NO, STRATUM_ID,  substr(TRIP||''-''||SET_NO,1,15) UNIQUE_ID,
     To_number(''1'') countCFV, To_number(''1'') countSets, VESSEL , TRIPCD_ID,
     CFV, TRIP, NAFAREA_ID, NAFDIV,
     HAULCCD_ID, P1LONG, P1LAT, P2LONG, P2LAT, P3LONG, P3LAT, P4LONG, P4LAT,
     SLONGITUDE, SLATITUDE, ELONGITUDE, ELATITUDE, SDAYTIME, P1TIME, P2TIME,
     EDAYTIME, P3TIME, P4TIME, SOAKMINP3P2, to_number(DURATION) DURATION, LENLLKM, NK_HOOKS,
     GEAR_LEN_M, MAINLINE_TYPE, MAINLINE_MAT, MAINLINE_DIAM, MAINLINE_WTAVG_KG, MAINLINE_LEN,
     P1DEPTH, P2DEPTH, P3DEPTH, P4DEPTH, SDEPTH, EDEPTH, GEARCD_ID,
     HOOKCD_ID, HOOKSIZE, NUM_HOOK_HAUL, HOOKSPACINGM, SPECSCD_ID, VESS_ID,
     TRIP_ID,  FISHSET_ID, SETCD_ID, SET_TYPE
      FROM ISD_INF WHERE to_number(DURATION) > 179 AND NK_HOOKS > 0.500 AND SET_TYPE != ''SURVEY - FIXED''
        UNION
  SELECT  * FROM ISD_INF_A4)';
execute immediate 'grant select on ISD_INF2  to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

/* now create a table of average durations and nK_hooks for the tripcd_id 7058 data */

execute immediate
    'CREATE OR REPLACE VIEW ISD_INF_A7058 AS
        (SELECT  TRIP, count (*) countSets, SUM(NVL(DURATION, 600)) t_duration,
             avg(NVL(DURATION, 600)) avg_duration,
             SUM(NVL(DURATION, 600) *  NVL(NK_HOOKS, 1.0))/SUM( NVL(NK_HOOKS, 1.0)) T_adj_DURATION,
             AVG( NVL(NK_HOOKS, 1.0)) NK_HOOKS, SUM(NK_HOOKS) tot_nK_hooks
       FROM ISD_INF
        WHERE tripcd_id = 7058
        GROUP BY  trip)';

execute immediate 'grant select on ISD_INF_A7058  to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

END;

PROCEDURE make_02_ISD_LF
/*
Procedure Name:		make_02_ISD_LF
Author:				Pierre C. Brien
Date Written:		2003/02/04
Function:
   MAKE TABLE ISD_LF - combines the longline length frequencies for trip type 7057
   in the ISDB and the length freuqncies for the trip type 7058 in the port samples database.

Modification History (as Date YYYY/MM/DD  initials - change description):
	2003/02/04		conversion from script to PLSQL package using DNS
	2003/04/15		final changes to implement ISD_ALLCAT and ISD_ALLLF


*/
IS
	dbms_sql_feedback       INTEGER;
	success                 BOOLEAN := FALSE;
BEGIN

begin
	execute immediate 'DROP snapshot ISD_LF';
exception when others then null;
end;
	
execute immediate
    'CREATE snapshot ISD_LF refresh on demand
       AS SELECT
              t.Trip,
              f.Set_No,
              t.tripcd_id,
       	      f.setcd_id,
       	      c.speccd_id,
       	      s.common species,
              c.Est_Num_Caught,
              a.SexCd_Id,
              l.Fish_Length,
              l.Num_At_Length,
              f.FishSet_id,
              c.Catch_Id
         FROM
              observer_ISTrips t,
              observer_ISFishSets f,
			  observer_ISCatches c,
       	      observer_ISsamples a,
       	      mflib_species_codes s,
              observer_ISFishLengths l
        WHERE s.research    = c.speccd_id
          AND t.Trip_Id     = f.Trip_Id
          AND f.FishSet_Id  = c.FishSet_Id
          AND c.Catch_Id    = a.Catch_Id
          AND a.Smpl_Id     = l.Smpl_Id
          AND NVL(f.HaulCCd_Id,1) IN (1,2,3)
          AND t.TripCd_Id   in (7057)
          AND t.Owner_Group in (''USER_HALIBUT'',''USER_JAVITECH'',''USER_ACD'')
   UNION
      SELECT
              t.Trip,
              f.Set_No,
              t.tripcd_id,
       	      f.setcd_id,
       	      c.speccd_id,
       	      s.common species,
              c.Est_Num_Caught,
              a.SexCd_Id,
              l.Fish_Length,
              l.Num_At_Length,
              f.FishSet_id,
              c.Catch_Id
         FROM
              observer_ISTrips t,
              observer_ISFishSets f,
			  observer_ISCatches c,
       	      observer_ISsamples a,
       	      mflib_species_codes s,
              observer_ISFishLengths l
        WHERE s.research    = c.speccd_id
          AND t.Trip_Id     = f.Trip_Id
          AND f.FishSet_Id  = c.FishSet_Id
          AND c.Catch_Id    = a.Catch_Id
          AND a.Smpl_Id     = l.Smpl_Id
		  and c.speccd_id   != 30
          AND NVL(f.HaulCCd_Id,1) IN (1,2,3)
          AND t.TripCd_Id   in (7058)
          AND t.Owner_Group in (''USER_HALIBUT'',''USER_JAVITECH'',''USER_ACD'')
   UNION
    SELECT  TRIP, SET_NO, 7058 TRIPCD_ID, SETCD_ID,SPECCD_ID, SPECIES, EST_NUM_CAUGHT, SEXCD_ID,
            FISH_LENGTH,  NUM_AT_LENGTH, FISHSET_ID, CATCH_ID
    FROM
        (SELECT
               substr(substr(a.remark,
        	          instr(remark,''J''),
        	          instr(a.remark,chr(79))-instr(a.remark,''J'')-1),1,11) trip,
        	   1            SET_NO,
        	   10           SETCD_ID,
        	   TO_NUMBER(a.species)   SPECCD_ID,
        	   ''HALIBUT(ATLANTIC)''  SPECIES,
        	   0            EST_NUM_CAUGHT,
        	   to_NUMBER(b.sex) SEXCD_ID,
        	   a.sample     SMPL_ID,
               b.lengroup   FISH_LENGTH,
        	   b.numatlen   num_at_length,
        	   -1           FISHSET_ID,
        	   -1           CATCH_ID,
        	   to_char(a.datelanded,''YYYY'') year,
        	   to_char(a.datelanded,''MM'') month,
               substr(d.description2,1,2) nafdiv
          FROM mfd_port_samples_gpsamples a,
               mfd_port_samples_gplengths b,
        	   mfd_port_samples_gpmarkets c,
        	   mfd_port_samples_gpuniq_area2 d
         where a.sample=b.sample
           and a.sample=c.sample
           and a.area=d.areacode
           and a.sampled=''U''
           and a.species=30
           and c.market=''6''
           and b.lengroup !=9999)';
							
execute immediate 'GRANT select on ISD_LF to VDC, VDC_DEV, mflib';
begin
execute immediate 'grant select on ISD_LF to WILSON, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';
exception when others then
    null;  
end;
END;


PROCEDURE make_03_ISD_CAT
/*
Procedure Name:		make_03_ISD_CAT
Author:				Pierre C. Brien
Date Written:		2003/02/04
Function:
   MAKE TABLE ISD_CAT

	This procedure creates ISD_CAT which is immediately updated to fill the
	computed field NUM_FROM_LF containing the count of the fish which were
	measured. This field NUM_FROM_LF is used as a quality control field.

Modification History (as Date YYYY/MM/DD  initials - change description):
	2003/02/04		conversion from script to PLSQL package using DNS
	2003/04/15		final changes to implement ISD_ALLCAT and ISD_ALLLF


*/
IS
	type cv_type is ref cursor;
	cv	 cv_type;
	dbms_sql_feedback       INTEGER;
	success                 BOOLEAN := FALSE;
	I_Trip					number;
	I_Set_No				number;
	I_Speccd_Id				number;
	I_Num_At_Length			number;
BEGIN

execute immediate 'DROP snapshot ISD_CAT';

execute immediate
    'CREATE snapshot ISD_CAT refresh on demand AS 
	select a.*, NVL(b.num_at_length, 0) num_from_LF 
	from ( 
		SELECT t.trip,
            f.set_no,
     	    f.setcd_id,
     	    t.tripcd_id,
            c.speccd_id,
     	    s.common species,
            c.est_combined_wt,
            est_num_caught,
            c.est_kept_wt,
            c.est_discard_wt,
            f.fishset_id,
            c.catch_id
		FROM observer_ISTrips t,
            observer_ISFishSets f,
     	    mflib_species_codes s,
            observer_ISCatches c	
		WHERE c.speccd_id=s.research 
		  AND t.trip_id=f.trip_id
		  AND c.fishset_id=f.fishset_id
		  AND NVL(f.HaulCCd_Id,1) IN (1,2,3)
		  AND t.TripCd_Id   in (7057, 7058)
		  AND t.Owner_Group in (''USER_HALIBUT'',''USER_JAVITECH'',''USER_ACD'')
		  AND setcd_id in (4,5,10) /* J.Black - changed 4 to 4,10 to include commercial index */
		) a, 
		(
		SELECT trip, set_no, speccd_id, SUM(NUM_AT_LENGTH) NUM_AT_LENGTH FROM ISD_LF
				group by trip, set_no, speccd_id
		) b
	WHERE a.speccd_id=b.speccd_id(+) 
	  AND a.trip=b.trip(+)
	  AND a.set_no=b.set_no(+)
		';

/*
execute immediate 'UPDATE ISD_CAT SET NUM_FROM_LF =
            (SELECT  SUM(NUM_AT_LENGTH) NUM_AT_LENGTH     FROM ISD_LF
             where   ISD_LF.TRIP = ISD_CAT.TRIP AND ISD_LF.SET_NO = ISD_CAT.SET_NO AND
                     ISD_CAT.SPECCD_ID = ISD_LF.SPECCD_ID)';
   FOR I_row in (SELECT ISD_LF.TRIP, ISD_LF.SET_NO, ISD_LF.SPECCD_ID, SUM(NUM_AT_LENGTH) NUM_AT_LENGTH
					FROM ISD_LF
                    GROUP BY  ISD_LF.TRIP, ISD_LF.SET_NO, ISD_LF.SPECCD_ID)  LOOP
       UPDATE ISD_CAT SET NUM_FROM_LF = I_ROW.NUM_AT_LENGTH
             where   I_row.TRIP = ISD_CAT.TRIP AND I_row.SET_NO = ISD_CAT.SET_NO AND
                     I_row.SPECCD_ID = ISD_CAT.SPECCD_ID;
       COMMIT;
   END LOOP;
   open cv for 
	'SELECT ISD_LF.TRIP, ISD_LF.SET_NO, ISD_LF.SPECCD_ID, SUM(NUM_AT_LENGTH) NUM_AT_LENGTH
		FROM ISD_LF
		GROUP BY  ISD_LF.TRIP, ISD_LF.SET_NO, ISD_LF.SPECCD_ID';
	LOOP	
		fetch cv into I_Trip, I_Set_No, I_Speccd_Id, I_Num_At_Length; 
	EXIT WHEN cv%NOTFOUND;
		execute immediate '
			UPDATE ISD_CAT SET NUM_FROM_LF = :NUM_AT_LENGTH
             where :TRIP = ISD_CAT.TRIP
			   AND :SET_NO = ISD_CAT.SET_NO 
			   AND :SPECCD_ID = ISD_CAT.SPECCD_ID
			' using I_Num_At_Length, I_Trip, I_Set_No, I_Speccd_Id;
       COMMIT;
   END LOOP;
*/
execute immediate  'grant select on ISD_CAT to vdc';
execute immediate  'grant select on ISD_CAT to vdc';
execute immediate 'grant select on ISD_CAT to mflib';
begin
	execute immediate 'grant select on ISD_CAT to WILSON, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';
exception when others then
	null;
end;



END;

PROCEDURE make_04_IS_CODE_TABS
/*
Procedure Name:		make_04_IS_CODE_TABS
Author:				Pierre C. Brien
Date Written:		2003/02/04
Function:
   This procedure creates the following code tables
        issettypeabbrev - set type abbreviations
        is_triptype     - trip type codes
        is_years        - years in tables for drop down list
        is_nafdivs      - NAFO division selection list
        is_areas        - combined area type table
        nafarea_id      - NAFO area ids selection list
        is_strata       - stratum ID's
        is_sex          - sex code selection list
        is_settype      - set type selection list
        all_spec_cat    - species selection list
        ISSTRAT         - stratum ID's and surface area (note definition is quite different
                          from research strata)

	This uses Dynamic Natural SQL to create the object named. For example
	the PROCEDURE make_00_ISD_LF creates the object ISD_LF

Modification History (as Date YYYY/MM/DD  initials - change description):
	2003/02/04		conversion from script to PLSQL package using DNS


*/
IS
	dbms_sql_feedback       INTEGER;
	success                 BOOLEAN := FALSE;
BEGIN

execute immediate
       'create or replace view issettypeabbrev as
        select  setcd_id,
                set_type,
                DECODE(setcd_id,1,''COMM'',2,''TLEN'',3,''TBYC'',4,''SVFX'',5,''SVRN'',
        		      6,''FCHO'',7,''CNPR'',8,''EXPL'',9,''EXPR'',10,''INDX'') abbrev
        from observer_issettypecodes ';

--MMM pre2015-10-15 execute immediate 'grant select on issettypeabbrev to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

execute immediate
       'create or replace view is_triptype
        as select * from observer_istriptypecodes 
        where tripcd_id in (7057,7058)';

--MMM pre2015-10-15 execute immediate 'grant select on is_triptype to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

execute immediate
        ' create or replace view is_years as
          select distinct year
          from ISD_INF  ';

--MMM pre2015-10-15execute immediate 'grant select on is_years to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

execute immediate
       'create or replace view is_nafdivs as
        select distinct substr(nafarea_id,1,2) nafdiv
        from ISD_INF                                    ';

execute immediate 'grant select on is_nafdivs to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

execute immediate
    'create or replace view is_areas as
     select type, area from (
     select 1 sortnum, ''all'' type, ''all'' area from dual
     union select 2 sortnum, ''NAFDIV'' , ''--NAFO Divisions--'' from dual
     union select distinct 2 sortnum,''NAFDIV'', nafdiv from ISD_INF
     union select 3 sortnum, ''NAFAREA_ID'', ''--Unit Areas--'' from dual
     union select distinct 3 sortnum,''NAFAREA_ID'', nafarea_id from ISD_INF
     union select 4 sortnum, ''STRATUM_ID'', ''--Survey Stratum--'' from dual
     union select distinct 4 sortnum, ''STRATUM_ID'', stratum_id from ISD_INF
            where stratum_id is not null
     order by 1, 3)  ';
execute immediate 'grant select on is_areas to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

execute immediate
    'create or replace view is_nafareas as
     select distinct nafarea_id
     from ISD_INF                            ';
execute immediate 'grant select on is_nafareas to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

execute immediate
    'create or replace view is_strata as
     select distinct stratum_id
     from ISD_INF
     where stratum_id is not null           ';
execute immediate 'grant select on is_strata to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

execute immediate 'create or replace view is_sex as
    select * from observer_issexcodes
     where sexcd_id in
     (select distinct sexcd_id from ISD_LF)';
execute immediate 'grant select on is_sex to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

execute immediate
    'create or replace view is_settype as
     select distinct *
       from issettypeabbrev
      where setcd_id IN (select distinct setcd_id from ISD_INF)';

execute immediate 'grant select on is_settype to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

/* Define a species selection list */
begin
execute immediate 'DROP snapshot ALL_SPEC_CAT';
exception when others then null;
end;

execute immediate
    'create snapshot all_spec_cat refresh on demand as
       select distinct speccd_id, species, tot_est_caught , nCatches from
      (select speccd_id, species, sum(est_num_caught) tot_est_caught , count (est_num_caught) nCatches FROM ISD_cat group by speccd_id, species)
       WHERE (tot_est_caught > 1000) or (nCatches > 100)';
-- old version    'create table all_spec_cat as select distinct speccd_id, species from ISD_cat';
execute immediate 'grant select on ALL_SPEC_CAT to VDC, VDC_DEV, mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

/* Halibut strata are defined differently than RV surveys */
begin
execute immediate 'DROP table ISSTRAT';
exception when others then null;
end;

execute immediate
    'CREATE table ISSTRAT ('||
    '     STRATUM_ID                               VARCHAR2(5),   '||
    '     AREA                                     NUMBER,        '||
    '     SWT                                      NUMBER )       ';
execute immediate   'INSERT INTO ISSTRAT   values(''1'', 52358, 0.19274)';
execute immediate   'INSERT INTO ISSTRAT   values(''2'',135897, 0.50027)';
execute immediate   'INSERT INTO ISSTRAT   values(''3'', 83393, 0.30699)';
/* stratum areas    1 -   52358
	                2 -  135897
	                3-    83393          total area 271648
*/
execute immediate 'grant select on isstrat to VDC, VDC_DEV, mflib';
begin
execute immediate 'grant select on isstrat to WILSON, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';
exception when others then
null;
end;


END;

PROCEDURE make_05_ISD_CAT2
/*
Procedure Name:		make_05_ISD_CAT2
Author:				Pierre C. Brien
Date Written:		2003/02/04
Function:
   MAKE TABLE ISD_CAT2

    This procedure uses the same logic as make_01_ISD_INF to sum split sets and average stations where
    multiple sets were taken. The initial ISD_CAT was prepared in make_03_ISD_CAT
	ISD_CAT_S4   summed   setcd_id = 4 - sum split sets
    ISD_CAT_A4   averaged setcd_id = 4 - average where more than one vessel per station per year
    ISD_CAT2     contains sets which were 3+ hours soak time and had at least 500 hooks


Modification History (as Date YYYY/MM/DD  initials - change description):
	2003/02/04		conversion from script to PLSQL package using DNS
	2003/04/15		final changes to implement ISD_ALLCAT and ISD_ALLLF


*/
IS
	dbms_sql_feedback       INTEGER;
	success                 BOOLEAN := FALSE;
BEGIN

/*
    where the same vessel has visited the same station assume that only a part set was recorded
    each time and sum the results (note that sets, even part sets, of less than 3 hours are not
    considered valid)
*/
execute immediate
    'CREATE OR REPLACE VIEW ISD_CAT_S4 AS
        (SELECT VESSEL, YEAR, STATION, STRATUM_ID, SPECCD_ID, SPECIES, TRIPCD_ID, SET_TYPE, SETCD_ID,  Count (*) CountSets,
         SUM (NK_HOOKS)  NK_HOOKS,     (SUM(DURATION *  NK_HOOKS)/SUM(NK_HOOKS)) DURATION,
         SUM(EST_COMBINED_WT)     EST_COMBINED_WT, SUM(EST_NUM_CAUGHT) EST_NUM_CAUGHT,
         SUM(EST_KEPT_WT) EST_KEPT_WT, SUM(EST_DISCARD_WT)  EST_DISCARD_WT , SUM(NUM_FROM_LF) NUM_FROM_LF
        FROM
           (SELECT c.*, S4.STRATUM_ID, S4.SET_TYPE,  S4.DURATION,  S4.NK_HOOKS
            FROM
               (SELECT VESSEL, YEAR, STATION, ISD_CAT.*
                    FROM ISD_INF, ISD_CAT
                    WHERE ISD_INF.TRIP = ISD_CAT.TRIP AND ISD_INF.SET_NO = ISD_CAT.SET_NO
                          AND ISD_INF.SETCD_ID = ISD_CAT.SETCD_ID) c,
                ISD_INF_S4 S4
            WHERE S4.Year = c.year and S4.vessel = c.vessel and s4.station = c.station) i_cat
        WHERE SET_TYPE = ''SURVEY - FIXED'' and Duration > 179
        GROUP BY VESSEL, YEAR, STATION, STRATUM_ID, SPECCD_ID, SPECIES, TRIPCD_ID, SET_TYPE,  SETCD_ID)';

execute immediate  'grant select on ISD_CAT_S4 to VDC, VDC_DEV, mflib';
begin
	execute immediate 'grant select on ISD_CAT_S4 to WILSON, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';
exception when others then
	null;
end;
/*
  where more than one boat per year average the catch by year and station
*/
execute immediate
    'CREATE OR REPLACE VIEW ISD_CAT_A4 AS
        SELECT  YEAR, STATION, TO_NUMBER(STATION) SETNO,  STRATUM_ID,  substr(YEAR||''-''||STATION,1,15) UNIQUE_ID,
         SPECCD_ID, SPECIES, TRIPCD_ID, SET_TYPE, SETCD_ID,
         countCFV,  VESSEL,
         EST_COMBINED_WT/countCFV    EST_COMBINED_WT,  EST_NUM_CAUGHT/countCFV EST_NUM_CAUGHT,
         EST_KEPT_WT/countCFV EST_KEPT_WT,  EST_DISCARD_WT/countCFV  EST_DISCARD_WT ,
         NUM_FROM_LF/countCFV NUM_FROM_LF
         FROM
         (SELECT  CountSets, countCFV, c.*
         FROM
           (SELECT  YEAR, STATION, TO_NUMBER(STATION) SETNO,  STRATUM_ID,  substr(YEAR||''-''||STATION,1,15) UNIQUE_ID,
             SPECCD_ID, SPECIES, TRIPCD_ID, SET_TYPE, SETCD_ID, MIN (VESSEL) VESSEL,
             SUM(EST_COMBINED_WT)     EST_COMBINED_WT, SUM(EST_NUM_CAUGHT) EST_NUM_CAUGHT,
             SUM(EST_KEPT_WT) EST_KEPT_WT, SUM(EST_DISCARD_WT)  EST_DISCARD_WT, SUM(NUM_FROM_LF) NUM_FROM_LF
             FROM ISD_CAT_S4
             WHERE  Duration > 179 AND NK_HOOKS > 0.500
             GROUP BY  YEAR, STATION, STATION, STRATUM_ID, SPECCD_ID, SPECIES, SPECIES, TRIPCD_ID, SET_TYPE, SETCD_ID) c,
           ISD_INF2
             WHERE c.unique_id = ISD_inf2.unique_id)';

execute immediate  'grant select on ISD_CAT_A4 to VDC, VDC_DEV, mflib';
begin
	execute immediate 'grant select on ISD_CAT_A4 to WILSON, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';
exception when others then
	null;
end;

/*
  now do a UNION to get the commercial index stuff too
*/
execute immediate
    'CREATE OR REPLACE VIEW ISD_CAT2 AS
        SELECT * FROM ISD_CAT_A4
        UNION
        (SELECT YEAR, STATION, SET_NO,  STRATUM_ID, UNIQUE_ID, SPECCD_ID, SPECIES,
         TRIPCD_ID, SET_TYPE, SETCD_ID, 1 countCFV, VESSEL,
          EST_COMBINED_WT, EST_NUM_CAUGHT, EST_KEPT_WT, EST_DISCARD_WT, NUM_FROM_LF
          FROM
        (SELECT VESSEL, YEAR, STATION,  STRATUM_ID, substr(ISD_INF.TRIP||''-''||ISD_INF.SET_NO,1,15) UNIQUE_ID, SET_TYPE, DURATION,  NK_HOOKS,  ISD_CAT.*
        FROM ISD_INF, ISD_CAT
        WHERE ISD_INF.TRIP = ISD_CAT.TRIP AND ISD_INF.SET_NO = ISD_CAT.SET_NO
              AND SET_TYPE != ''SURVEY - FIXED'' and Duration > 179 AND NK_HOOKS > 0.500))';

execute immediate  'grant select on ISD_CAT2 to VDC, VDC_DEV, mflib';
begin
	execute immediate 'grant select on ISD_CAT2 to WILSON, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';
exception when others then
	null;
end;

-- ****************
-- written: R Branton 2004/02/17
-- purpose: Halibut Industry Survey
--          weighed, counted and measured sample counts by trip type, set type and species

execute immediate
    'create or replace view IS_SAMPLING_SUMMARY as
        select
               b.trip_type, b.tripcd_id,
               c.set_type, c.setcd_id,
        	   a.species, a.speccd_id,
               d.sets,
               count(decode(a.EST_COMBINED_WT,0,NULL,a.EST_COMBINED_WT)) weighed,
               count(decode(a.EST_NUM_CAUGHT,0,NULL,a.EST_NUM_CAUGHT)) counted,
        	   count(decode(a.NUM_FROM_LF,0,NULL,a.NUM_FROM_LF)) measured
          from ISD_CAT2 a,
        	   observer_istriptypecodes b,
        	   observer_issettypecodes c,
        		(SELECT setcd_id, TRIPCD_ID, count(distinct unique_id) sets
        		   FROM ISD_cat2
        		  GROUP BY setcd_id, TRIPCD_ID) d
         WHERE a.setcd_id=c.setcd_id
           and a.tripcd_id=b.tripcd_id
           and a.setcd_id=d.setcd_id
           and a.tripcd_id=d.tripcd_id
        GROUP BY b.trip_type, b.tripcd_id,
                 c.set_type, c.setcd_id,  d.sets,
        		 a.species, a.speccd_id
        order by 1,2';

execute immediate  'comment on table is_sampling_summary is ''Weighed, counted and measured sample counts by trip type, set type and species.''';
--
-- need column comments for is_sampling_summary TRIP_TYPE   TRIPCD_ID SET_TYPE  SETCD_ID SPECIES   SPECCD_ID  SETS  WEIGHED  COUNTED  MEASURED
--MMM execute immediate  'grant select on IS_SAMPLING_SUMMARY to VDC, VDC_DEV, mflib';
begin
	execute immediate 'grant select on IS_SAMPLING_SUMMARY to WILSON, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';
exception when others then
	null;
end;

-- ****************
END;

PROCEDURE make_06_ISD_LF2
/*
PROCEDURE make_06_ISD_LF2

Procedure Name:		make_06_ISD_LF2
Author:				Pierre C. Brien
Date Written:		2003/02/04
Function:

    This procedure uses the same logic as make_01_ISD_INF to sum split sets and average stations where
    multiple sets were taken. The initial ISD_CAT was prepared in make_03_ISD_CAT
	ISD_LF_S4   summed   setcd_id = 4 - sum split sets
    ISD_LF_A4   averaged setcd_id = 4 - average where more than one vessel per station per year
    ISD_LF2     contains sets which were 3+ hours soak time and had at least 500 hooks

    ISD_ALLLF   The sub query named cart produces a complete set of trip, set, species, sex_cd stub rows
                These stub rows are used to perform an outer join with the catch data thus giving us
                the ability to compute catch rates averaged over all sets.

Modification History (as Date YYYY/MM/DD  initials - change description):
	2003/02/04		conversion from script to PLSQL package using DNS
	2003/04/15		final changes to implement ISD_ALLCAT and ISD_ALLLF


*/
IS
	dbms_sql_feedback       INTEGER;
	success                 BOOLEAN := FALSE;
BEGIN

execute immediate
    'CREATE OR REPLACE VIEW ISD_LF_S4 AS
        SELECT LF4.VESSEL, LF4.YEAR, LF4.STATION, LF4.STRATUM_ID, LF4.SPECCD_ID, LF4.SPECIES, LF4.FISH_LENGTH,
               LF4.SEXCD_ID , LF4.SET_TYPE, LF4.SETCD_ID, LF4.TRIPCD_ID, ISD_INF_S4.NK_HOOKS, ISD_INF_S4.DURATION,
               LF4.EST_NUM_CAUGHT, LF4.NUM_AT_LENGTH
        FROM
        (SELECT VESSEL, YEAR,TRIP, SET_NO, STATION, STRATUM_ID, SPECCD_ID, SPECIES, FISH_LENGTH, SEXCD_ID , SET_TYPE, SETCD_ID,
          TRIPCD_ID,  SUM (NK_HOOKS)  NK_HOOKS,     (SUM(DURATION *  NK_HOOKS)/SUM(NK_HOOKS)) DURATION,
         SUM(EST_NUM_CAUGHT)   EST_NUM_CAUGHT,  SUM(NUM_AT_LENGTH)  NUM_AT_LENGTH
        FROM (SELECT VESSEL, YEAR, STATION, STRATUM_ID, SET_TYPE,  DURATION,  NK_HOOKS,  ISD_LF.*
        FROM ISD_INF, ISD_LF
        WHERE ISD_INF.FISHSET_ID = ISD_LF.FISHSET_ID  AND ISD_INF.setcd_id  = 4) i_lf
        WHERE SET_TYPE = ''SURVEY - FIXED'' and Duration > 179
        GROUP BY VESSEL, YEAR,TRIP, SET_NO, STATION, STRATUM_ID, SPECCD_ID,
                 SPECIES, FISH_LENGTH, SEXCD_ID, SET_TYPE, SETCD_ID, TRIPCD_ID) lf4,
        ISD_INF_S4
        WHERE ISD_INF_S4.YEAR = LF4.YEAR AND ISD_INF_S4.VESSEL = LF4.VESSEL AND ISD_INF_S4.STATION = LF4.STATION';

execute immediate
'CREATE OR REPLACE VIEW ISD_LF_A4 AS
     SELECT  A4.YEAR, A4.STATION,  A4.SETNO,  A4.STRATUM_ID,  A4.UNIQUE_ID,
             A4.SPECCD_ID, A4.SPECIES, A4.SET_TYPE, A4.SETCD_ID,  A4.TRIPCD_ID, FISH_LENGTH, SEXCD_ID,
          EST_NUM_CAUGHT /countCFV  EST_NUM_CAUGHT ,  NUM_AT_LENGTH /CountCFV    NUM_AT_LENGTH ,
            HOOK_NUM_AT_LENGTH /countCFV   HOOK_NUM_AT_LENGTH ,
            BOTH_NUM_AT_LENGTH /countCFV   BOTH_NUM_AT_LENGTH
        FROM
        (SELECT  YEAR, STATION, TO_NUMBER(STATION) SETNO,  STRATUM_ID,  substr(YEAR||''-''||STATION,1,15) UNIQUE_ID,
                 SPECCD_ID, SPECIES, SET_TYPE, SETCD_ID,  TRIPCD_ID, FISH_LENGTH, SEXCD_ID,
         SUM(EST_NUM_CAUGHT) EST_NUM_CAUGHT,  SUM(NUM_AT_LENGTH)  NUM_AT_LENGTH ,
           SUM(NUM_AT_LENGTH/nK_Hooks) HOOK_NUM_AT_LENGTH ,
           SUM((NUM_AT_LENGTH/nK_Hooks)*(600/Duration))  BOTH_NUM_AT_LENGTH
         FROM ISD_LF_S4
         WHERE  Duration > 179 AND NK_HOOKS > 0.500
         GROUP BY  YEAR, STATION, STATION, STRATUM_ID, SPECCD_ID, SPECIES,
                   SET_TYPE, SETCD_ID, TRIPCD_ID, FISH_LENGTH, SEXCD_ID) A4,
         ISD_INF_A4
         WHERE A4.unique_id = ISD_INF_A4.UNIQUE_ID';

execute immediate
'CREATE OR REPLACE VIEW ISD_LF2 AS
        SELECT  YEAR, STATION, SETNO, STRATUM_ID, UNIQUE_ID, SPECCD_ID, SPECIES, SET_TYPE, SETCD_ID, TRIPCD_ID,
            FISH_LENGTH, SEXCD_ID, EST_NUM_CAUGHT, NUM_AT_LENGTH, HOOK_NUM_AT_LENGTH , BOTH_NUM_AT_LENGTH
        FROM ISD_LF_A4
        UNION
        (SELECT YEAR, STATION, SET_NO,  STRATUM_ID, UNIQUE_ID, SPECCD_ID, SPECIES,   SET_TYPE, SETCD_ID, TRIPCD_ID,
          FISH_LENGTH,  SEXCD_ID,
          EST_NUM_CAUGHT, NUM_AT_LENGTH,  (NUM_AT_LENGTH/nK_Hooks) HOOK_NUM_AT_LENGTH,
          ((NUM_AT_LENGTH/nK_Hooks) *(Duration/600))BOTH_NUM_AT_LENGTH
          FROM
        (SELECT VESSEL, YEAR, STATION,  STRATUM_ID, substr(ISD_INF.TRIP||''-''||ISD_INF.SET_NO,1,15) UNIQUE_ID,
                SET_TYPE, DURATION,  NK_HOOKS,  ISD_LF.*
        FROM ISD_INF, ISD_LF
        WHERE ISD_INF.TRIP = ISD_LF.TRIP AND ISD_INF.SET_NO = ISD_LF.SET_NO
              AND ISD_INF.TRIPCD_ID = 7057
              AND SET_TYPE != ''SURVEY - FIXED'' and Duration > 179 AND NK_HOOKS > 0.500))
        UNION
        (SELECT YEAR, STATION, SET_NO,  STRATUM_ID, UNIQUE_ID, SPECCD_ID, SPECIES,   SET_TYPE, SETCD_ID, TRIPCD_ID,
          FISH_LENGTH,  SEXCD_ID,
          EST_NUM_CAUGHT, NUM_AT_LENGTH,  (NUM_AT_LENGTH/nK_Hooks) HOOK_NUM_AT_LENGTH,
          ((NUM_AT_LENGTH/nK_Hooks) *(T_ADJ_DURATION/600))BOTH_NUM_AT_LENGTH
          FROM
        (SELECT VESSEL, YEAR, STATION,  STRATUM_ID, substr(ISD_INF.TRIP||''-''||ISD_INF.SET_NO,1,15) UNIQUE_ID,
                SET_TYPE, ISD_INF_A7058.T_ADJ_DURATION,  ISD_INF_A7058.NK_HOOKS,  ISD_LF.*
        FROM ISD_INF_A7058, ISD_INF, ISD_LF
        WHERE ISD_INF_A7058.TRIP = ISD_LF.TRIP  AND T_ADJ_DURATION > 179 AND ISD_INF_A7058.NK_HOOKS > 0.500 AND
              ISD_INF.TRIP = ISD_LF.TRIP AND ISD_INF.SET_NO = ISD_LF.SET_NO ))';

execute immediate 'GRANT select on ISD_LF2 to VDC, VDC_DEV, mflib';
begin
	execute immediate 'grant select on ISD_LF2 to WILSON, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';
exception when others then
	null;
end;

--  note inner query cart which provides the means to convert the sparse matrix ISD_LF2 to one containing all
--       cells.
execute immediate
   'CREATE OR REPLACE VIEW ISD_ALLLF AS
    SELECT cart.year yr, cart.nafarea_id, cart.nafdiv, cart.trip, cart.set_no, cart.unique_id,
           tt.trip_type,   cart.tripcd_id, cart.set_type, cart.setcd_id,
           cart.station, cart.stratum_id, cart.slongitude lon, cart.slatitude lat,
            cart.duration, cart.nk_hooks, cart.countsets, cart.countcfv, cart.speccd_id, code.common species,
            cart.sexcd_id, cart.FISH_LENGTH,
              NVL(est_num_caught,decode(est_combined_wt, NULL, 0, NULL))          rawnum,
              NVL(NUM_AT_LENGTH ,decode(est_combined_wt, NULL, 0, NULL))           NUM_AT_LENGTH,
              NVL(HOOK_NUM_AT_LENGTH ,decode(est_combined_wt, NULL, 0, NULL)) HOOK_NUM_AT_LENGTH,
              NVL(BOTH_NUM_AT_LENGTH ,decode(est_combined_wt, NULL, 0, NULL)) BOTH_NUM_AT_LENGTH
         FROM (SELECT i.*, spec.speccd_id, spec.sexcd_id, spec.fish_length
                 FROM ISD_INF2 i,
                      (select l.speccd_id, s.sexcd_id , l.fish_length
                        FROM
                            (select distinct speccd_id, fish_length FROM ISD_lf where speccd_id in
                                        (SELECT SPECCD_ID from is_species_measured)) l,
                            (select distinct speccd_id, sexcd_id    FROM ISD_lf where speccd_id in
                                        (SELECT SPECCD_ID from is_species_measured)) s
                          WHERE l.speccd_id = s.speccd_id) spec
    		   ) cart,
              (SELECT lf.*,est_combined_wt FROM ISD_LF2 lf, ISD_CAT2 c
                     WHERE lf.unique_id=c.unique_id AND lf.speccd_id=c.speccd_id)lf,
              is_triptype tt,
    		  mflib_species_codes code
        WHERE cart.speccd_id=code.research
    	  AND cart.unique_id=lf.unique_id(+)
    	  AND cart.tripcd_id = tt.tripcd_id
          AND cart.fish_length=lf.fish_length(+)
          AND cart.speccd_id=lf.speccd_id(+)
    	  AND cart.sexcd_id=lf.sexcd_id(+)';


-- MMM execute immediate 'GRANT select on ISD_ALLLF to VDC, VDC_DEV, mflib';
begin
	execute immediate 'grant select on ISD_ALLLF to WILSON, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';
exception when others then
	null;
end;

END;


PROCEDURE make_07_IS_SPEC_MEAS
/*
Procedure Name:		make_07_IS_SPECIES_MEASUREMENTS
Author:				Pierre C. Brien
Date Written:		2003/02/04
Function:
   MAKE   VIEW IS_SPECIES_WEIGHED
   MAKE   VIEW IS_SPECIES_COUNTED
   MAKE   VIEW IS_SPECIES_MEASURED

	This uses Dynamic Natural SQL to create the object named. For example
	the PROCEDURE make_00_ISD_LF creates the object ISD_LF

Modification History (as Date YYYY/MM/DD  initials - change description):
	2003/02/04		conversion from script to PLSQL package using DNS


*/
IS
	dbms_sql_feedback       INTEGER;
	success                 BOOLEAN := FALSE;
BEGIN
execute immediate
   'create or replace view is_species_weighed as
    select species, speccd_id,
           round(sum(decode(setcd_id,1,est_combined_wt,0))) comm_wgt,
           round(sum(decode(setcd_id,4,est_combined_wt,0))) svfx_wgt,
           round(sum(decode(setcd_id,5,est_combined_wt,0))) svrn_wgt,
           round(sum(decode(setcd_id,10,est_combined_wt,0)))indx_wgt,
    	   round(sum(est_combined_wt)) all_wgt
    from ISD_CAT
    group by species, speccd_id                                       ';

execute immediate 'grant select on is_species_weighed to vdc';
execute immediate 'grant select on is_species_weighed to vdc';
execute immediate 'grant select on is_species_weighed to mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

execute immediate
   'create or replace view is_species_counted as
    select species, speccd_id,
        round(sum(decode(setcd_id,1,est_num_caught,0))) comm_num,
        round(sum(decode(setcd_id,4,est_num_caught,0))) svfx_num,
        round(sum(decode(setcd_id,5,est_num_caught,0))) svrn_num,
        round(sum(decode(setcd_id,10,est_num_caught,0))) indx_num,
    	   round(sum(est_num_caught)) all_num
    from ISD_CAT
    where est_num_caught is not null
    group by species, speccd_id   ';

execute immediate  'grant select on is_species_counted to vdc';
execute immediate  'grant select on is_species_counted to vdc';
execute immediate  'grant select on is_species_counted to mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

execute immediate
    'create or replace view is_species_measured as
     select distinct species, speccd_id,
          round(sum(decode(setcd_id,1,num_at_length,0))) comm_meas,
          round(sum(decode(setcd_id,4,num_at_length,0))) svfx_meas,
          round(sum(decode(setcd_id,5,num_at_length,0))) svrn_meas,
          round(sum(decode(setcd_id,10,num_at_length,0))) indx_meas,
     	   round(sum(num_at_length)) all_meas
     from ISD_LF
     group by species, speccd_id                                      ';

execute immediate 'grant select on is_species_measured to vdc';
execute immediate 'grant select on is_species_measured to vdc';
execute immediate 'grant select on is_species_measured to mflib, wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

END;


PROCEDURE make_08_ISD_ALLCAT
/*
Procedure Name:		make_16_ISD_ALLCAT
Author:				Pierre C. Brien
Date Written:		2003/02/04
Function:
   MAKE  VIEW ISD_ALLCAT

	This uses Dynamic Natural SQL to create the object named. For example
	the PROCEDURE make_00_ISD_LF creates the object ISD_LF

Modification History (as Date YYYY/MM/DD  initials - change description):
	2003/02/04		conversion from script to PLSQL package using DNS
	2003/04/15		final changes to implement ISD_ALLCAT and ISD_ALLLF


*/
IS
	dbms_sql_feedback       INTEGER;
	success                 BOOLEAN := FALSE;
BEGIN
--  note inner query cart which provides the means to convert the sparse matrix ISD_CAT2 to one containing all
--       cells.

execute immediate
    'CREATE OR REPLACE VIEW ISD_allcat AS
    SELECT cart.year yr, cart.nafarea_id, cart.nafdiv, cart.trip, cart.set_no, cart.unique_id, cart.tripcd_id,
           tt.trip_type,  cart.set_type, cart.setcd_id,
           cart.station, cart.stratum_id, cart.slongitude lon, cart.slatitude lat,
    	   cart.duration, cart.nk_hooks, cart.countsets, cart.countcfv, cart.species, cart.speccd_id,
    	   NVL(est_combined_wt,0)rawwgt,
           NVL(est_combined_wt,0)/cart.nK_hooks hookwgt,
      	   (NVL(est_combined_wt,0)/cart.nK_hooks)*(600/cart.duration) bothwgt,
      	   NVL(est_num_caught,decode(est_combined_wt, NULL, 0, NULL)) rawnum,
      	   NVL(est_num_caught,decode(est_combined_wt, NULL, 0, NULL))/cart.nK_hooks hooknum,
      	   (NVL(est_num_caught,decode(est_combined_wt, NULL, 0, NULL))/cart.nK_hooks)*(600/cart.duration) bothnum,
    	   NVL(NUM_FROM_LF,decode(est_combined_wt, NULL,  0, NULL))    RAWNUM_LF,
      	   NVL(NUM_FROM_LF,decode(est_combined_wt, NULL,  0, NULL))/cart.nK_hooks hooknum_lf,
      	   (NVL(NUM_FROM_LF,decode(est_combined_wt, NULL, 0, NULL))/cart.nK_hooks)*(600/cart.duration) bothnum_lf
     FROM (SELECT i.*, c.speccd_id, c.species
    	     FROM ISD_INF2 i,
    	          all_spec_cat c) cart,
    	  IS_TripType tt,
    	  ISD_CAT2 catch
    WHERE cart.unique_id=catch.unique_id(+)
      AND cart.speccd_id=catch.speccd_id(+)
      AND cart.tripcd_id=tt.tripcd_id';

--MMM execute immediate 'grant select on ISD_allcat to vdc with grant option';
--MMM execute immediate 'grant select on ISD_allcat to vdc with grant option';
--MMM execute immediate 'grant select on ISD_allcat to mflib with grant option';
/* MMM
begin
--MMM	execute immediate 'grant select on ISD_allcat to WILSON, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';
exception when others then
	htp.p(sqlerrm);
	null;
end;
*/

execute immediate
    'create or replace view is_species_strat as
     select distinct speccd_id, species
     from ISD_allcat                               ';

--MMM execute immediate 'grant select on is_species_strat to vdc with grant option';
--MMM execute immediate 'grant select on is_species_strat to vdc with grant option';
--MMM execute immediate 'grant select on is_species_strat to mflib with grant option';
--MMM execute immediate 'grant select on is_species_strat to wilson, denheyer,mcmahonm, wringeb, hubleyb,smiths, wilsong';

END;


PROCEDURE make_99_IS_COMMENTS
/*
Procedure Name:		make_99_IS_COMMENTS
Author:				Pierre C. Brien
Date Written:		2003/02/04
Function:
   MAKE  MAKE ALL TABLE AND COLUMN COMMENTS


	This uses Dynamic Natural SQL to create the object named. For example
	the PROCEDURE make_00_ISD_LF creates the object ISD_LF

Modification History (as Date YYYY/MM/DD  initials - change description):
	2003/02/04		conversion from script to PLSQL package using DNS
	2003/04/15		final changes to implement ISD_ALLCAT and ISD_ALLLF


*/
IS
	dbms_sql_feedback       INTEGER;
	success                 BOOLEAN := FALSE;
BEGIN

execute immediate 'comment on MATERIALIZED VIEW  ISD_INF is ''set postion, depth, etc... by year, season, vessel, trip and set_no''';
execute immediate 'comment on MATERIALIZED VIEW  ISD_cat is ''numbers and weights by trip, set_no and species''';
execute immediate 'comment on MATERIALIZED VIEW  ISD_INF2 is ''Set postion, depth, etc... by year, season, vessel, trip and set_no selected for soak time and no. of hooks criteria''';
execute immediate 'comment on MATERIALIZED VIEW  ISD_cat2 is ''Numbers and weights by year, season, vessel, trip, set_no and species selected for soak time and no. of hooks criteria''';
execute immediate 'comment on MATERIALIZED VIEW  ISD_lf is ''numbers at length by year, vessel, trip, set_no, species''';
execute immediate 'comment on MATERIALIZED VIEW  ISD_lf2 is ''numbers at length by year, station, set_no, stratum, species selected for soak time and no. of hooks criteria''';

execute immediate 'comment on table is_areas is ''list of areas by type e.g. nafo division, unit area and strata''';
execute immediate 'comment on table is_nafareas is ''list of statitical units actually occupied''';
execute immediate 'comment on table is_nafdivs is ''list of nafo divisions actually occupied''';
--execute immediate 'comment on table is_seasons is ''list of seasons when survey actually took place''';
execute immediate 'comment on table is_settype is ''list of used set types codes, descriptions and abbreviations''';
execute immediate 'comment on table issettypeabbrev is ''list of all set types codes, descriptions and abbreviations''';
execute immediate 'comment on table is_sex is ''list of sex codes and descriptions''';
execute immediate 'comment on table is_species_counted is ''list of species actually counted on the survey plus total counts by set type''';
execute immediate 'comment on table is_species_measured is ''list of species actually measured for length on the survey plus total counts by set type''';
execute immediate 'comment on table is_species_weighed is ''list of species actually weighed on the survey plus total weights by set type''';
execute immediate 'comment on table is_species_strat is ''list of species actually weighed on the random stratified surveys''';
execute immediate 'comment on table is_strata is ''list of survey strata actually occupied''';
execute immediate 'comment on table isstrat   is ''list of survey strata, stratum area and stratum weighting factor''';
execute immediate 'comment on table is_years is ''list of years when survey actually took place''';
execute immediate 'comment on table is_triptype is ''list of halibut survey trip types''';

execute immediate 'comment on table ISD_ALLCAT is ''all set and catch data including zeroes''';
execute immediate 'comment on table ISD_ALLLF is  ''all set and size composition data including zeroes''';

execute immediate 'comment on column ISD_inf.Year                  is ''Year  of trip        ''';
execute immediate 'comment on column ISD_inf.season                is ''season               ''';
execute immediate 'comment on column ISD_inf.Cfv                   is ''vessel identification    ''';
execute immediate 'comment on column ISD_inf.Trip                  is ''trip identification            ''';
execute immediate 'comment on column ISD_inf.Set_No                is ''Set number       ''';
execute immediate 'comment on column ISD_inf.NafArea_Id            is ''NAFO area identification   ''';
execute immediate 'comment on column ISD_inf.NafDiv                is ''NAFO division ''';
execute immediate 'comment on column ISD_inf.Stratum_Id            is ''Stratum identification ''';
execute immediate 'comment on column ISD_inf.station               is ''station -- id for fishing station  ''';
execute immediate 'comment on column ISD_inf.setcd_id              is ''type of set coded''';
execute immediate 'comment on column ISD_inf.set_type              is ''set_type   type of set  ''';
execute immediate 'comment on column ISD_inf.HaulCCd_Id            is ''Haul Condition Code''';
execute immediate 'comment on column ISD_inf.SpecSCd_Id            is ''Species Code   ''';
execute immediate 'comment on column ISD_inf.SLongitude            is ''Longitude at start of set''';
execute immediate 'comment on column ISD_inf.SLatitude             is ''Latitude at start of set ''';
execute immediate 'comment on column ISD_inf.ELongitude            is ''Longitude at end of set''';
execute immediate 'comment on column ISD_inf.ELatitude             is ''Latitude at end of set          ''';
execute immediate 'comment on column ISD_inf.SDayTime              is ''Start day with time in hours as decimal fraction''';
execute immediate 'comment on column ISD_inf.EDayTime              is ''End day with time in hours as decimal fraction''';
execute immediate 'comment on column ISD_inf.Duration              is ''set or set duration in minutes - durationp4p1''';
execute immediate 'comment on column ISD_inf.LenLLkm               is ''Length of the Long Line in km.''';
execute immediate 'comment on column ISD_inf.nK_hooks              is ''No. of hooks /1000''';
execute immediate 'comment on column ISD_inf.SDepth                is ''depth at start of set''';
execute immediate 'comment on column ISD_inf.EDepth                is ''depth at end of set''';
execute immediate 'comment on column ISD_inf.GearCd_Id             is ''gear code id''';
execute immediate 'comment on column ISD_inf.FishSet_Id            is ''Fish Set Identifier''';


execute immediate 'comment on column ISD_cat.trip                  IS ''trip identification''';
execute immediate 'comment on column ISD_cat.set_no                IS ''set number''';
execute immediate 'comment on column ISD_cat.setcd_id              IS ''type of set coded ''';
execute immediate 'comment on column ISD_cat.speccd_id             IS ''species code ''';
execute immediate 'comment on column ISD_cat.species               IS ''species common name ''';
execute immediate 'comment on column ISD_cat.est_combined_wt       IS ''est_combined_wt ''';
execute immediate 'comment on column ISD_cat.est_num_caught        IS '' calc_num_caught or est_num_caught''';
execute immediate 'comment on column ISD_cat.est_kept_wt           IS ''est_kept_wt ''';
execute immediate 'comment on column ISD_cat.est_discard_wt        IS ''est_discard_wt ''';
execute immediate 'comment on column ISD_cat.fishset_id            IS ''Fish Set Identification''';
execute immediate 'comment on column ISD_cat.catch_id              IS ''catch_id - link to catch row ''';



execute immediate 'comment on column is_species_weighed.species      IS ''species common name ''';
execute immediate 'comment on column is_species_weighed.speccd_id    IS ''species code ''';
execute immediate 'comment on column is_species_weighed.comm_wgt     IS ''weight of fish from Commercial''';
execute immediate 'comment on column is_species_weighed.svfx_wgt     IS ''weight of fish from svfx''';
execute immediate 'comment on column is_species_weighed.svrn_wgt     IS ''weight of fish from Survey Random''';
execute immediate 'comment on column is_species_weighed.indx_wgt     IS ''weight of fish from indx''';
execute immediate 'comment on column is_species_weighed.all_wgt      IS ''Weight from all sources ''';



execute immediate 'comment on column is_species_counted.species      IS ''species common name ''';
execute immediate 'comment on column is_species_counted.speccd_id    IS ''species code ''';
execute immediate 'comment on column is_species_counted.comm_num     IS ''Count of fish from Commercial''';
execute immediate 'comment on column is_species_counted.svfx_num     IS ''Count of fish from svfx''';
execute immediate 'comment on column is_species_counted.svrn_num     IS ''Count of fish from Survey Random''';
execute immediate 'comment on column is_species_counted.indx_num     IS ''Count of fish from indx''';
execute immediate 'comment on column is_species_counted.all_num      IS ''Count from all sources ''';


execute immediate 'comment on column ISD_lf.Trip              IS ''Trip Id''';
execute immediate 'comment on column ISD_lf.Set_No            IS ''Set Number''';
execute immediate 'comment on column ISD_lf.setcd_id          IS ''Set type code''';
execute immediate 'comment on column ISD_lf.speccd_id         IS ''Species code''';
execute immediate 'comment on column ISD_lf.species           IS ''Species common name''';
execute immediate 'comment on column ISD_lf.Est_Num_Caught    IS ''Estimated number caught at length''';
execute immediate 'comment on column ISD_lf.SexCd_Id          IS ''Sex code''';
execute immediate 'comment on column ISD_lf.Fish_Length       IS ''Fish length''';
execute immediate 'comment on column ISD_lf.Num_At_Length     IS ''Number at length''';
execute immediate 'comment on column ISD_lf.FishSet_id        IS ''Fish Set ID link to fishSet row''';
execute immediate 'comment on column ISD_lf.Catch_Id          IS ''catch_id - link to catch row ''';


execute immediate 'comment on column is_species_measured.species          IS ''Species common name ''';
execute immediate 'comment on column is_species_measured.speccd_id        IS ''Species code ''';
execute immediate 'comment on column is_species_measured.comm_meas        IS ''number measured Commercial''';
execute immediate 'comment on column is_species_measured.svfx_meas        IS ''number measured svfx''';
execute immediate 'comment on column is_species_measured.svrn_meas        IS ''number measured Survey Random''';
execute immediate 'comment on column is_species_measured.indx_meas        IS ''number measured indx''';
execute immediate 'comment on column is_species_measured.all_meas         IS ''number measured all samples''';


execute immediate 'comment on column ISD_allcat.yr               IS ''Year  of trip''';
execute immediate 'comment on column ISD_allcat.nafarea_id       IS ''NAFO area identification''';
execute immediate 'comment on column ISD_allcat.nafdiv           IS ''NAFO division''';
execute immediate 'comment on column ISD_allcat.trip             IS ''trip identification''';
execute immediate 'comment on column ISD_allcat.set_no           IS ''Set number''';
execute immediate 'comment on column ISD_allcat.unique_id        IS ''Unique ID (generated)''';
execute immediate 'comment on column ISD_allcat.tripcd_id        IS ''trip code identification''';
execute immediate 'comment on column ISD_allcat.trip_type        IS ''trip type ''';
execute immediate 'comment on column ISD_allcat.set_type         IS ''type of set''';
execute immediate 'comment on column ISD_allcat.setcd_id         IS ''type of set - coded''';
execute immediate 'comment on column ISD_allcat.station          IS ''station -- id for fishing station''';
execute immediate 'comment on column ISD_allcat.stratum_id       IS ''Stratum identification''';
execute immediate 'comment on column ISD_allcat.lon              IS ''Longitude''';
execute immediate 'comment on column ISD_allcat.lat              IS ''Latitude''';
execute immediate 'comment on column ISD_allcat.duration         IS ''set or set duration in minutes - durationp4p1''';
execute immediate 'comment on column ISD_allcat.nk_hooks         IS ''No. of hooks /1000''';
execute immediate 'comment on column ISD_allcat.countsets        IS ''number of sets''';
execute immediate 'comment on column ISD_allcat.countcfv         IS ''number of vessels''';
execute immediate 'comment on column ISD_allcat.species          IS ''Species Name''';
execute immediate 'comment on column ISD_allcat.speccd_id        IS ''Species code''';
execute immediate 'comment on column ISD_allcat.rawwgt           IS ''raw weight caught''';
execute immediate 'comment on column ISD_allcat.hookwgt          IS ''weight caught adjusted for no. of hooks ''';
execute immediate 'comment on column ISD_allcat.bothwgt          IS ''Weight caught adjusted for hooks and soak time ''';
execute immediate 'comment on column ISD_allcat.rawnum           IS ''raw numbers caught''';
execute immediate 'comment on column ISD_allcat.hooknum          IS ''numbers caught adjusted for no. of hooks''';
execute immediate 'comment on column ISD_allcat.bothnum          IS ''caught adjusted for hooks and soak time ''';
execute immediate 'comment on column ISD_allcat.rawnum_lf        IS ''raw numbers measured''';
execute immediate 'comment on column ISD_allcat.hooknum_lf       IS ''numbers measured adjusted for no. of hooks''';
execute immediate 'comment on column ISD_allcat.bothnum_lf       IS ''numbers measured adjusted for hooks and soak time ''';


execute immediate 'comment on column ISD_alllf.yr                IS ''Year  of trip''';
execute immediate 'comment on column ISD_alllf.nafarea_id        IS ''NAFO area identification''';
execute immediate 'comment on column ISD_alllf.nafdiv            IS ''NAFO division''';
execute immediate 'comment on column ISD_alllf.trip              IS ''trip identification''';
execute immediate 'comment on column ISD_alllf.set_no            IS ''Set number''';
execute immediate 'comment on column ISD_alllf.unique_id         IS ''Unique ID (generated)''';
execute immediate 'comment on column ISD_alllf.trip_type         IS ''trip type ''';
execute immediate 'comment on column ISD_alllf.tripcd_id         IS ''trip code identification''';
execute immediate 'comment on column ISD_alllf.set_type          IS ''type of set''';
execute immediate 'comment on column ISD_alllf.setcd_id          IS ''type of set - coded''';
execute immediate 'comment on column ISD_alllf.station           IS ''station -- id for fishing station''';
execute immediate 'comment on column ISD_alllf.stratum_id        IS ''Stratum identification''';
execute immediate 'comment on column ISD_alllf.lon               IS ''Longitude''';
execute immediate 'comment on column ISD_alllf.lat               IS ''Latitude''';
execute immediate 'comment on column ISD_alllf.duration          IS ''set or set duration in minutes - durationp4p1''';
execute immediate 'comment on column ISD_alllf.nk_hooks          IS ''No. of hooks /1000''';
execute immediate 'comment on column ISD_alllf.countsets         IS ''number of sets''';
execute immediate 'comment on column ISD_alllf.countcfv          IS ''number of vessels''';
execute immediate 'comment on column ISD_alllf.species           IS ''Species Name''';
execute immediate 'comment on column ISD_alllf.speccd_id         IS ''Species code''';
execute immediate 'comment on column ISD_alllf.sexcd_id              IS ''sex coded''';
execute immediate 'comment on column ISD_alllf.fish_length           IS ''Slength of fish in cm.''';
execute immediate 'comment on column ISD_alllf.rawnum                IS ''raw number caught ''';
execute immediate 'comment on column ISD_alllf.num_at_length         IS ''number measured at length''';
execute immediate 'comment on column ISD_alllf.hook_num_at_length    IS ''number measured adjusted for no. of hooks''';
execute immediate 'comment on column ISD_alllf.both_num_at_length    IS ''number measured adjusted for no. of hooks and soak time''';

END;


PROCEDURE ISDB_Halibut_refresh_form
/*
PROCEDURE Materialized Views Refresh Selection

Procedure Name:		ISDB_Halibut_refresh_form
Author:				Pierre C. Brien
Date Written:		2003/10/07
Function:
	This routine allows users with appropriate permissions to cause the IDSB_Halibut materialized views to
	be refreshed so as to include any new data. This permits statistical views to be brought up to date
	on demand and not force the user to wait for automated refresh procedures (none yet implemented for
	ISDB_Halibut)

	Input parameter - USER_ID the user_id is input and passed to the refresh routine as a parameter
	this allows for potentiallt tracking usage by user. Currently no log is maintained for this system.

Modification History (as Date YYYY/MM/DD  initials - change description):
	2001/09/24		initial test version (NWAGS)
	2002/03/28		dummy version for IML
	                - this should be a temporary fix while we determine
	                the future procedural topology of
    2003/10/07      NWAGS refresh procedure used as skeleton

*/
IS
BEGIN
	vdc.Mwmfdutil.restore_state(state, staff, divn_abbr);

 	vdc.mwmfdutil.prolog ('ISDB Halibut - Materialized Views Refresh Administration ');
	htp.strong('<FONT color=teal size=5> ISDB Halibut - Materialized Views Refresh Administration  </font><br><br>','');

	htp.formOpen('ISDB_HALIBUT.ISDB_halibut_refresh.Edit_source');
	htp.formHidden('User_Id_in',staff.id);
    htp.formSubmit('',vdc.mwmfdutil.msgtranslate('Run edits'));
	htp.formclose;

	htp.formOpen('ISDB_HALIBUT.ISDB_halibut_refresh.MV_refresh_All');
	htp.formHidden('User_Id_in',staff.username);
	for row in status loop
		if row.complete = 'Y' then
			htp.formSubmit('Button_in',vdc.mwmfdutil.msgtranslate('Refresh All Materialized Views'));
		else
			htp.formSubmit('Button_in',vdc.mwmfdutil.msgtranslate('Refresh All Materialized Views'), cattributes=>'disabled');
		end if;
	end loop;		  
	htp.formclose;

	vdc.mwmfdutil.epilog;
END;

PROCEDURE MV_refresh_All(
                User_Id_in      IN      varchar2  DEFAULT NULL,
                Button_in       IN      varchar2  default null)
/*
Procedure Name:		MV_refresh_All
Author:				Pierre C. Brien
Date Written:		2003/10/08
Function:
   Refresh all MV's

	This procedure replaces the create all SQL script so the refresh can be accomplished
	from a Web enabled command button. Note that it is important to keep the create_all
	script and this procedure in synchrony.

Modification History (as Date YYYY/MM/DD  initials - change description):

*/
IS
BEGIN	  
	htp.htmlopen;
	htp.bodyopen;
	htp.strong('<FONT color=teal size=5> ISDB_Halibut - Materialized Views Refresh in process  </font><br><br>','');
	htp.bodyclose;
	htp.htmlclose;

 	DBMS_REFRESH.refresh('ISDB_HALIBUT.HalibutSurvey');

	htp.htmlopen;
	htp.bodyopen;
	htp.p(vdc.mwmfdutil.msgTranslate('Refresh complete'));
	htp.bodyclose;
	htp.htmlclose;
EXCEPTION WHEN OTHERS THEN
	htp.p(sqlerrm);
END;

PROCEDURE create_All
/*
Procedure Name:		MV_refresh_All
Author:				D. Broughton
Date Written:		2005/10/26
Function:
   Refresh all MV's

	This procedure providees the initial creation of snapshots & views

Modification History (as Date YYYY/MM/DD  initials - change description):
	2005/10/26	db	Make almost all Tables into Snapshots - copied from Pierre's 'MV_REFRESH_ALL'
					to eliminate requirement for separate "create all" script


*/
IS
	dbms_sql_feedback       INTEGER;
	success                 BOOLEAN := FALSE;
BEGIN

-- 00)  TABLE ISLF (BOOTSTRAP version) only need to run this one when starting from scratch
-- make_00_ISD_LF;


-- 01)  TABLE ISINF  creates
--                   TABLE ISD_INF
--                   TABLE ISD_INF_S4
--                   TABLE ISD_INF_A4
--                   VIEW ISD_INF2
              ISDB_halibut_refresh.make_01_ISD_INF;

-- 02)  TABLE ISD_LF
              ISDB_halibut_refresh.make_02_ISD_LF;

-- 03)  TABLE  ISD_CAT
              ISDB_halibut_refresh.make_03_ISD_CAT;

-- 04)  VIEWS IS_CODE_TABS
--                   view issettypeabbrev
--                   view is_triptype
--                   view is_years
--                   view is_nafdivs
--                   view is_areas
--                   view is_nafareas
--                   view is_strata
--                   view is_sex
--                   view is_settype
--                   TABLE ISSTRAT

              ISDB_halibut_refresh.make_04_IS_CODE_TABS;

-- 05)  VIEW ISD_CAT2
--                   VIEW ISD_CAT_S4 AS
--                   VIEW ISD_CAT_A4 AS
--                   VIEW ISD_CAT2 AS
              ISDB_halibut_refresh.make_05_ISD_CAT2;

-- 07)  VIEW IS_SPECIES_MEASUREMENTS
--                   view is_species_weighed
--                   view is_species_counted
--                   view is_species_measured
              ISDB_halibut_refresh.make_07_IS_SPEC_MEAS;

-- 06)  VIEW  ISD_LF2
--                   VIEW ISD_LF_S4
--                   VIEW ISD_LF_A4
--                   VIEW ISD_LF2
--                   VIEW ISD_ALLLF
              ISDB_halibut_refresh.make_06_ISD_LF2;

-- 08)  VIEW ISD_ALLCAT
--                   VIEW ISD_allcat AS
--                   view is_species_strat as
              ISDB_halibut_refresh.make_08_ISD_ALLCAT;

-- 27)  TABLE AND COLUMN COMMENTS
              ISDB_halibut_refresh.make_99_IS_COMMENTS;

--	refresh group to perform Snapshot refreshes in order			 
	 dbms_refresh.make('ISDB_HALIBUT.HalibutSurvey',
					list=>'ISDB_HALIBUT.ISD_INF, ISDB_HALIBUT.ISD_INF_S4, ISDB_HALIBUT.ISD_INF_A4,'||
							'ISDB_HALIBUT.ISD_LF, ISDB_HALIBUT.ISD_CAT, ISDB_HALIBUT.ALL_SPEC_CAT',
					next_date=>to_date('40000101','yyyymmdd'),
					lax=>true,
					interval=>'to_date(''40000101'',''yyyymmdd'')'
					);
	commit;

	htp.strong('<FONT color=teal size=5> ISDB_Halibut - Materialized Views Refresh Successful  </font><br><br>','');

END create_all;	
		
PROCEDURE refresh_all_isdb_halibut
/*
Procedure Name:		refresh_all_isdb_halibut
Author:				J. Black
Date Written:		2007/01/17
Function:
   Refresh all MV's

	This procedure providees the initial creation of snapshots & views

Modification History (as Date YYYY/MM/DD  initials - change description):
	2007/01/17	jb	Took mv_refresh_all and made this copy which doesn't make the refresh group


*/
IS
	dbms_sql_feedback       INTEGER;
	success                 BOOLEAN := FALSE;
BEGIN

-- 00)  TABLE ISLF (BOOTSTRAP version) only need to run this one when starting from scratch
-- make_00_ISD_LF;


-- 01)  TABLE ISINF  creates
--                   TABLE ISD_INF
--                   TABLE ISD_INF_S4
--                   TABLE ISD_INF_A4
--                   VIEW ISD_INF2
              ISDB_halibut_refresh.make_01_ISD_INF;

-- 02)  TABLE ISD_LF
              ISDB_halibut_refresh.make_02_ISD_LF;

-- 03)  TABLE  ISD_CAT
              ISDB_halibut_refresh.make_03_ISD_CAT;

-- 04)  VIEWS IS_CODE_TABS
--                   view issettypeabbrev
--                   view is_triptype
--                   view is_years
--                   view is_nafdivs
--                   view is_areas
--                   view is_nafareas
--                   view is_strata
--                   view is_sex
--                   view is_settype
--                   TABLE ISSTRAT

              ISDB_halibut_refresh.make_04_IS_CODE_TABS;

-- 05)  VIEW ISD_CAT2
--                   VIEW ISD_CAT_S4 AS
--                   VIEW ISD_CAT_A4 AS
--                   VIEW ISD_CAT2 AS
              ISDB_halibut_refresh.make_05_ISD_CAT2;

-- 07)  VIEW IS_SPECIES_MEASUREMENTS
--                   view is_species_weighed
--                   view is_species_counted
--                   view is_species_measured
              ISDB_halibut_refresh.make_07_IS_SPEC_MEAS;

-- 06)  VIEW  ISD_LF2
--                   VIEW ISD_LF_S4
--                   VIEW ISD_LF_A4
--                   VIEW ISD_LF2
--                   VIEW ISD_ALLLF
              ISDB_halibut_refresh.make_06_ISD_LF2;

-- 08)  VIEW ISD_ALLCAT
--                   VIEW ISD_allcat AS
--                   view is_species_strat as
              ISDB_halibut_refresh.make_08_ISD_ALLCAT;

-- 27)  TABLE AND COLUMN COMMENTS
              ISDB_halibut_refresh.make_99_IS_COMMENTS;

	commit;


END refresh_all_isdb_halibut;	
		

-- function summarize return number
-- IS
-- BEGIN 
-- 	return owa_Util.tablePrint('ISDB_HALIBUT.ISD_SUMMARY'
-- 						, cattributes=>'border="1" cellpadding="0" cellspacing="0" align="center" class="hilite"'
--  						, ccolumns=>'*'
-- 						, cclauses=>'order by year desc, source');
-- 	vdc.mwdd_dynamic.list2('ISDB Halibut survey summary'
-- 							,'ISDB_HALIBUT.ISD_SUMMARY'
-- 							,'*'
-- 							,'order by year desc, source');
--END;

	
procedure accept_edits
	( 
	refresh in varchar2 default null,
	accept	in varchar2 default null
	) is
	moreRows boolean;
begin
	vdc.Mwmfdutil.restore_state(state, staff, divn_abbr);
	
	vdc.mwmfdutil.prolog('Halibut Survey Data Edits');

	for edit in status loop
		if edit.complete = 'Y' 
		or edit.complete = 'P' and NVL(edit.user_id,0) != staff.id then
			edit.complete := 'Y';	--	second approver makes it complete
			htp.header(1,vdc.mwmfdutil.msgTranslate('Edits approved: ')|| edit.year); 
		else
			edit.complete := 'P';	--	first approver sets it pending
			htp.header(1,vdc.mwmfdutil.msgTranslate('Edits are pending approval: ')|| edit.year); 
		end if;				   
	
		update survey_edit_status 	--	set new status - unless it's already complete
		set user_id = staff.id
		,	complete = edit.complete
		where NVL(complete,'N') != 'Y' and year = edit.year ;
	
		if edit.complete = 'Y' then
			htp.formOpen('ISDB_HALIBUT.ISDB_halibut_refresh.MV_refresh_All');
			htp.formHidden('User_Id_in',staff.username);
			htp.formSubmit('Button_in',vdc.mwmfdutil.msgtranslate('Finalize'));
			htp.formclose;

			moreRows := owa_Util.tablePrint('ISDB_HALIBUT.ISD_SUMMARY'
						, cattributes=>'border="1" cellpadding="0" cellspacing="0" align="center" class="hilite"'
 						, ccolumns=>'*'
						, cclauses=>'order by year desc, source');
		end if;	
	end loop;						
	
	vdc.mwmfdutil.epilog;
end;


procedure Edit_source (
--                Button_in       IN      varchar2,
                User_Id_in      IN      varchar2 default null
	) AS
	button	varchar2(4000);
		
	cursor trips is
		select distinct trip_id 
			from observer_ISTrips t
			WHERE t.TripCd_Id in (7057)
			  AND t.Owner_Group in ('USER_HALIBUT','USER_JAVITECH','USER_ACD')
			  AND to_char(t.landing_date,'yyyy') = edit.year
			  AND not exists (
				select f.Trip_Id from observer_ISFishSets f
				where f.trip_id = t.trip_id
				  AND NVL(f.HaulCCd_Id,1) IN (1,2,3)
				);
	cursor unmatched_trips is
		SELECT  distinct t.Trip
			FROM observer_ISTrips t
			,	observer_ISFishSets f
			,	observer_iscatches c
			,	observer_ISsamples a
			,	mflib_species_codes s
			,	observer_ISFishLengths l
		    WHERE s.research    = c.speccd_id                                                    
			  AND t.Trip_Id     = f.Trip_Id                                                      
			  AND f.FishSet_Id  = c.FishSet_Id                                                   
			  AND c.Catch_Id    = a.Catch_Id 
			  AND a.Smpl_Id     = l.Smpl_Id 
			  AND c.speccd_id   = 30                                                     
			  AND NVL(f.HaulCCd_Id,1) IN (1,2,3)                                                 
			  AND t.TripCd_Id   in (7058)                                                         
			  AND t.Owner_Group in ('USER_HALIBUT','USER_JAVITECH','USER_ACD')
		minus         
		SELECT distinct
               RTRIM(substr(substr(a.remark,
        	          instr(remark,'J'),
        	          instr(a.remark,chr(79))-instr(a.remark,'J')-1),1,11),' .') trip
			FROM mfd_port_samples_gpsamples a
			,	mfd_port_samples_gplengths b
			,	mfd_port_samples_gpmarkets c
			,	mfd_port_samples_gpuniq_area2 d
			where a.sample=b.sample
			  and a.sample=c.sample
			  and a.area=d.areacode
			  and a.sampled='U'
			  and c.market='6'
			  and b.lengroup !=9999
		;
											 
	cursor fishsets is
		select distinct f.Trip_Id, f.FishSet_Id 
			from observer_ISTrips t
			,	 observer_ISFishSets f
			WHERE t.TripCd_Id in (7057)
			  AND t.Owner_Group in ('USER_HALIBUT','USER_JAVITECH','USER_ACD')
			  AND t.Trip_Id     = f.Trip_Id
			  AND to_char(t.landing_date,'yyyy') = edit.year
			  AND NVL(f.HaulCCd_Id,1) IN (1,2,3)
			  AND not exists (
				select FishSet_Id from observer_ISCatches c where f.FishSet_Id  = c.FishSet_Id
				);
	cursor catches is
		select distinct f.Trip_Id, f.FishSet_Id, c.Catch_Id
			from observer_ISTrips t
			,	 observer_ISFishSets f
			,	 observer_ISCatches c
			WHERE t.TripCd_Id in (7057)
			  AND t.Owner_Group in ('USER_HALIBUT','USER_JAVITECH','USER_ACD')
			  AND t.Trip_Id     = f.Trip_Id
			  AND to_char(t.landing_date,'yyyy') = edit.year
			  AND NVL(f.HaulCCd_Id,1) IN (1,2,3)
			  AND f.FishSet_Id  = c.FishSet_Id
			  AND c.speccd_id	= 30		--	only halibuts!
			  AND not exists (
				select a.catch_id from observer_ISsamples a where c.Catch_Id = a.Catch_Id
				)
		;
	cursor issamples is
		select distinct f.Trip_Id, f.FishSet_Id, c.Catch_Id, a.Smpl_Id
			from observer_ISTrips t
			,	 observer_ISFishSets f
			,	 observer_ISCatches c
			,	 observer_ISsamples a
			WHERE t.TripCd_Id in (7057)
			  AND t.Owner_Group in ('USER_HALIBUT','USER_JAVITECH','USER_ACD')
			  AND t.Trip_Id     = f.Trip_Id
			  AND to_char(t.landing_date,'yyyy') = edit.year
			  AND NVL(f.HaulCCd_Id,1) IN (1,2,3)
			  AND f.FishSet_Id  = c.FishSet_Id
			  AND c.speccd_id	= 30		--	only halibuts!
			  AND c.Catch_Id    = a.Catch_Id
			  AND not exists (
				select l.Smpl_Id from observer_ISFishLengths l where a.Smpl_Id = l.Smpl_Id 
				)
		;
	cursor vessels is  
		select distinct t.vess_id
			from observer_ISTrips t
			WHERE t.TripCd_Id in (7057)
			  AND t.Owner_Group in ('USER_HALIBUT','USER_JAVITECH','USER_ACD')
			  AND to_char(t.landing_date,'yyyy') = edit.year
			  AND not exists (
				select v.vess_id from observer_ISvessels v where v.vess_id = t.vess_id
			  )
		;
	cursor subsampling is
		SELECT c.trip, c.set_no, c.speccd_id, nFishMeas, est_num_caught 
			FROM ISD_CAT C
			,	(SELECT trip, Set_no, speccd_id, SUM(Num_At_Length) nFishMeas 
					FROM (SELECT SUB_SAMPLE_FLAG
								, t.Trip
								, f.Set_No
								, t.tripcd_id
								, f.setcd_id
								, c.speccd_id
								, s.common species
								, c.Est_Num_Caught
								, a.SexCd_Id
								, l.Fish_Length
								, l.Num_At_Length
								, f.FishSet_id
								, c.Catch_Id                                                                     
							FROM observer_ISTrips t
							,	observer_ISFishSets f
							,	observer_iscatches c
							,	observer_ISsamples a
							,	mflib_species_codes s
							,	observer_ISFishLengths l                                                       
							WHERE s.research    = c.speccd_id                                                    
							  AND t.Trip_Id     = f.Trip_Id                                                      
							  AND f.FishSet_Id  = c.FishSet_Id                                                   
							  AND c.Catch_Id    = a.Catch_Id 
							  AND a.Smpl_Id     = l.Smpl_Id 
							  AND NVL(f.HaulCCd_Id,1) IN (1,2,3)                                                 
							  AND t.TripCd_Id   in (7057)                                                         
							  AND t.Owner_Group in ('USER_HALIBUT','USER_JAVITECH','USER_ACD')
							  AND c.speccd_id	= 30  
							  and to_char(t.landing_date,'yyyy') = edit.year
						)
					GROUP BY  trip, Set_no, speccd_id
					) lf
				WHERE LF.TRIP = C.TRIP
				  AND LF.SET_NO = C.SET_NO 
				  AND LF.SPECCD_ID = C.SPECCD_ID
				  AND lf.nFishMeas < c.est_num_caught
		;

-- 	cursor gpsamples is
-- 		;
-- 		
	cursor stations is
		select to_number(f.station) station 
			from observer_ISTrips t
			,	 observer_ISFishSets f
			WHERE t.TripCd_Id in (7057)
			  AND t.Owner_Group in ('USER_HALIBUT','USER_JAVITECH','USER_ACD')
			  AND t.Trip_Id     = f.Trip_Id
			  AND NVL(f.HaulCCd_Id,1) IN (1,2,3) 
			group by to_number(f.station)
			having max(to_char(landing_date,'yyyy') ) < edit.year			  		
		;	   			
	
	function css_class (rowno integer) return varchar2 IS
	BEGIN
		if mod(rowno,2) = 0 then
			return 'class="hilite"';
		else
			return '';
		end if;
	END;
BEGIN		  
	vdc.mwmfdutil.prolog('Halibut Survey Data Edits');
	
	--	find the current processing year.  
	--	If the current year is complete, and it is a new year, open the new year
	open status;
	fetch status into edit;
	close status; 

	if edit.year is null 
	or edit.complete = 'Y' and edit.year < to_char(sysdate, 'yyyy') then
		edit.year	  := NVL(edit.year + 1, to_char(sysdate, 'yyyy'));			 
		edit.complete := 'N';
		insert into survey_edit_status (year)
			values(edit.year);	
		edit.user_id	  := null;
	end if;
 
	if edit.complete = 'Y' then
		button := htf.formSubmit('refresh', vdc.mwmfdutil.msgtranslate('Refresh All Materialized Views'));
	elsif edit.user_id = user_id_in then
		button := vdc.mwmfdutil.msgtranslate('You have approved these edits<BR>(edits must be approved by two users)');
	elsif edit.complete = 'P' then		--	ie, it's been approved BY SOMEONE ELSE already
		button := htf.formSubmit('refresh', vdc.mwmfdutil.msgtranslate('Refresh All Materialized Views'));
	else
		button := htf.formSubmit('accept',vdc.mwmfdutil.msgtranslate('Accept edits'));
	end if;
				
	htp.header(1,vdc.mwmfdutil.msgTranslate('Halibut Survey Data Edits: ')|| edit.year); 
	
	htp.tableopen('', calign=>'CENTER');
		htp.tablerowopen;
			htp.tableheader(htf.formopen('ISDB_HALIBUT.ISDB_halibut_refresh.accept_edits')
				|| button
				|| htf.formClose); 
		htp.tablerowclose;
	htp.tableClose;
	
	htp.tableopen('', cattributes=>'width="100%" align="CENTER"');
	htp.tablecaption(vdc.mwmfdutil.msgTranslate('IStrips without corresponding ISFishSets'));
	for row in trips loop
		if trips%rowcount = 1 then
			htp.tablerowopen;
				htp.tableHeader('Trip_ID');
			htp.tablerowclose;
		end if;										   
		htp.tablerowopen(cattributes=>css_class(trips%rowcount));
			htp.tabledata(row.trip_id);
		htp.tablerowclose;
	end loop;			  
	htp.tableClose;
	
	htp.tableopen('', cattributes=>'width="100%" align="CENTER"');
	htp.tablecaption(vdc.mwmfdutil.msgTranslate('IStrips without Port Samples'));
	for row in unmatched_trips loop
		if unmatched_trips%rowcount = 1 then
			htp.tablerowopen;
				htp.tableHeader('Trip');
			htp.tablerowclose;
		end if;										   
		htp.tablerowopen(cattributes=>css_class(unmatched_trips%rowcount));
			htp.tabledata(row.trip);
		htp.tablerowclose;
	end loop;			  
	htp.tableClose;
	
	htp.tableopen('', cattributes=>'width="100%" align="CENTER"');
	htp.tablecaption(vdc.mwmfdutil.msgTranslate('ISFishSets without corresponding ISCatches'));
	for row in fishsets loop
		if fishsets%rowcount = 1 then
			htp.tablerowopen;
				htp.tableHeader('Trip_ID');
				htp.tableHeader('FishSet_Id');
			htp.tablerowclose;
		end if;										   
		htp.tablerowopen(cattributes=>css_class(fishsets%rowcount));
			htp.tabledata(row.trip_id);
			htp.tableData(row.FishSet_Id);
		htp.tablerowclose;
	end loop;			  
	htp.tableClose;
	
	htp.tableopen('', cattributes=>'width="100%" align="CENTER"');
	htp.tablecaption(vdc.mwmfdutil.msgTranslate('ISCatches without corresponding ISSamples'));
	for row in catches loop
		if catches%rowcount = 1 then
			htp.tablerowopen;
				htp.tableHeader('Trip_ID');
				htp.tableHeader('FishSet_Id');
				htp.tableHeader('Catch_Id');
			htp.tablerowclose;
		end if;										   
		htp.tablerowopen(cattributes=>css_class(catches%rowcount));
			htp.tabledata(row.trip_id);
			htp.tableData(row.FishSet_Id);
			htp.tableData(row.Catch_Id);
		htp.tablerowclose;
	end loop;			  
	htp.tableClose;
	
	htp.tableopen('', cattributes=>'width="100%" align="CENTER"');
	htp.tablecaption(vdc.mwmfdutil.msgTranslate('ISSamples without corresponding ISFishLengths'));
	for row in issamples loop
		if issamples%rowcount = 1 then
			htp.tablerowopen;
				htp.tableHeader('Trip_ID');
				htp.tableHeader('FishSet_Id');
				htp.tableHeader('Catch_Id');
				htp.tableHeader('Smpl_Id');
			htp.tablerowclose;
		end if;										   
		htp.tablerowopen(cattributes=>css_class(issamples%rowcount));
			htp.tabledata(row.trip_id);
			htp.tableData(row.FishSet_Id);
			htp.tableData(row.Catch_Id);
			htp.tableData(row.Smpl_Id);
		htp.tablerowclose;
	end loop;			  
	htp.tableClose;
	
	htp.tableopen('', cattributes=>'width="100%" align="CENTER"');
	htp.tablecaption(vdc.mwmfdutil.msgTranslate('ISTrips without corresponding ISVessels'));
	for row in vessels loop
		if vessels%rowcount = 1 then
			htp.tablerowopen;
				htp.tableHeader('Vess_ID');
			htp.tablerowclose;
		end if;										   
		htp.tablerowopen(cattributes=>css_class(vessels%rowcount));
			htp.tabledata(row.vess_id);
		htp.tablerowclose;
	end loop;			  
	htp.tableClose;	 
	
	htp.tableopen('', cattributes=>'width="50%" align="CENTER"');
	htp.tablecaption(vdc.mwmfdutil.msgTranslate('Stations not found in current year''s data'));
	for row in stations loop
		if stations%rowcount = 1 then
			htp.tablerowopen;
				htp.tableHeader('Station');
			htp.tablerowclose;
		end if;										   
		htp.tablerowopen(cattributes=>css_class(stations%rowcount));
			htp.tabledata(row.station);
		htp.tablerowclose;
	end loop;			  
	htp.tableClose;	 
	
	htp.tableopen('', cattributes=>'width="100%" align="CENTER"');
	htp.tablecaption(vdc.mwmfdutil.msgTranslate('Incomplete sampling'));
	for row in subsampling loop
		if subsampling%rowcount = 1 then
			htp.tablerowopen;
				htp.tabledata('SUB_SAMPLE_FLAG'); 
				htp.tabledata('TRIP'); 
				htp.tabledata('SET_NO'); 
				htp.tabledata('SPECCD_ID'); 
				htp.tabledata('NFISHMEAS'); 
				htp.tabledata('EST_NUM_CAUGHT');
			htp.tablerowclose;
		end if;										   
		htp.tablerowopen(cattributes=>css_class(subsampling%rowcount));
--			htp.tabledata(row.SUB_SAMPLE_FLAG); 
			htp.tabledata(row.trip); 
			htp.tabledata(row.set_no); 
			htp.tabledata(row.speccd_id); 
			htp.tabledata(row.nFishMeas); 
			htp.tabledata(row.est_num_caught);
		htp.tablerowclose;
	end loop;			  
	htp.tableClose;	 
	
	vdc.mwmfdutil.epilog;	
END Edit_source;

END;