--"Automate" HAST ISDB Tagging to Temp Tag Data Model

-- Added October 14, 2010 - LTB
-- Before using below update/add from ISDB check that sequences are there are contain "valid" last_number value
/*
REMARKS last_number should be greater than or equal to max(vessel_id);
select last_number from user_sequences where sequence_name = 'TAG_VESSELCODES_ID_SQ';
select max(vessel_id) from temp_tag_vesselcodes;

REMARKS last_number should be greater than or equal to max(vessel_id);
select last_number from user_sequences where sequence_name = 'TAG_CONTACTCODES_ID_SQ';
select max(contact_id) from temp_tag_contactcodes;

REMARKS last_number should be greater than or equal to max(nafo_id);
select last_number from user_sequences where sequence_name = 'TAG_NAFOCODES_ID_SQ';
select max(nafo_id) from temp_tag_nafocodes;

REMARKS last_number should be greater than or equal to max(trip_id);
select last_number from user_sequences where sequence_name = 'TAG_TRIPS_ID_SQ';
select max(trip_id) from temp_tag_trips;

REMARKS last_number should be greater than or equal to max(animal_id);
select last_number from user_sequences where sequence_name = 'TAG_ANIMALS_ID_SQ';
select max(animal_id) from temp_tag_animals;

REMARKS last_number should be greater than or equal to max(tag_id);
select last_number from user_sequences where sequence_name = 'TAG_TAGS_ID_SQ';
select max(tag_id) from temp_tag_tags;

REMARKS last_number should be greater than or equal to max(event_id);
select last_number from user_sequences where sequence_name = 'TAG_EVENTS_ID_SQ';
select max(event_id) from temp_tag_events;
*/

--Before using/updating from manual information, should run this to update/add from ISDB.

/*
REMARKS  Update by S Bond June 29, 2010
REMARKS
REMARKS  Reason:  Issue discovered when G Wilson attempted to run.  1)  The sequence Tag_Trips_ID_Sq had to be dropped and recreated, the last number assigned was noted as being 3, the actual last number used was 53?
REMARKS   Issue 2:  The script at the Bottom, Inster into Temp_Tag_Events wouldn't work, through a number of SQL errors that needed resolving, as well as some logic corrected.  Note that this would never have run.


REMARKS  Lenore Bajona August 2, 2010
REMARKS  Issue when G Wilson attempted to run insert temp_tag_tags; sequence used by before insert trigger was gone thus trigger invalid.
REMARKS  Recreated using correct start with value:   CREATE SEQUENCE TAG_TAGS_ID_SQ INCREMENT BY 1 START WITH 4233 MAXVALUE 999999999999999999999999 NOCYCLE NOCACHE;

REMARKS  Update by Lenore Bajona August 17, 2010
REMARKS  Vessels Unique constraint (name, cfv, country_id) violated as one already loaded vessel changes/fixed max(brake_hp) from 26 to 261
REMARKS  *** Need to chanage insert to minus where name, cfv, cntrycd already loaded, then add to edit system, update where already there but different other values ??
REMARKS  update temp_tag_vesselcodes set brake_hp=261 where vessel_cfv = 106068;
REMARKS  insert into temp_tag_events not enough values error, added values () and removed nulls

REMARKS Update by Mike McMahon, Nov 22, 2012
REMARKS Same as last time - Vessels Unique contraint (name||cfv||country_id) violated.  
REMARKS Discovered 3 vessels that had slightly different data in temp_tags than in observer
REMARKS Updated temp_tag records to match observer db, and script got past that point (see below)

REMARKS Update by Mike McMahon, Nov 28, 2012
REMARKS Script failed on "insert into temp_tag_tags"the where it failed with another unique constraint violation on TAGS_NOTYPEPRGMPREFCLR_UK
REMARKS Noticed that all failed records were 2012, and that they would contain null values for prog_id due to "case when yr=2010 then 4 else null end progid"
REMARKS Probably not OK to have a null there, since it makes up part of the constraint that is being violated.
REMARKS Adding case statement for 2012, applying prog_id of 7 ("when yr=2012 then 7")
REMARKS Changed >=2010 to = 2012 in the interest of getting the most recent tags in, and hopefully skipping over the exceptions table
REMARKS Update by Mike McMahon, Nov 29, 2012
REMARKS Following the discovery of the contraint problem on the temp_tag_events table, I modified the  "TAGS_NOTYPEPRGMPREFCLR_UK" constraint to also include
REMARKS datasoource_id and the script was able to run for >=2010.  Booya.

REMARKS  Update by Mike McMahon, Nov 29, 2012
REMARKS Got to the TEMP_TAG_EVENTS update, but crashed on an invalid number conversion.
REMARKS Traced the problem back to cases where the datasource_key had only partial information
REMARKS DATASOURCE_KEY is parsed, and bits of it are used to relate to other tables.
REMARKS Modified these comparisons to ensure that the value exists prior to comparing them.  EG. went from:
REMARKS AND SUBSTR(an.datasource_key, instr(an.datasource_key,'CATCH_ID:')  +9, instr(SUBSTR(an.datasource_key, instr(an.datasource_key,'CATCH_ID:')+9),',')-1)     = a.catch_id
REMARKS to
REMARKS AND CASE when instr(an.datasource_key,'CATCH_ID:')>0 then SUBSTr(an.datasource_key, instr(an.datasource_key,'CATCH_ID:')  +9, instr(SUBSTr(an.datasource_key, instr(an.datasource_key,'CATCH_ID:')+9),',')-1) else null end = a.catch_id

REMARKS  Update by Mike McMahon, Nov 29, 2012
REMARKS Fix above helped, but got another Unique constraint violation 
REMARKS Believe it failed on tag_id 5833, eventtype_id=1, animal_id=2259, datasource_id=2 (datasource_key is blank).  
REMARKS Important aspect is that there is an identical record to this, but with datasource_id=1.
REMARKS Modified unique contraint to include datasource_id
REMARKS Also, tag_id 4848,4856,5809,5816 and5817 had different contact_ids between the 2 sources - 285 and 316)  Both pointed to the same person, so I changed instances of 285 to the more recent 316

REMARKS  Update by Mike McMahon, Aug 3, 2017
REMARKS  Added a MAX() to contact_codes for temp_tag_trips, due to trip duplications.  These duplications were due to different contact_ids being provided that pointed to the same individual
(i.e. repetition in the TEMP_TAG_CONTACT_CODES table)
*/
INSERT
INTO temp_tag_vesselcodes
  (
    VESSEL_NAME,
    VESSEL_CFV,
    COUNTRY_ID,
    LENGTH,
    BRAKE_HP,
    TONC_ID
  )
  (SELECT v.vessel_name,
      v.cfv,
      MIN(v.ctrycd_id),
      ROUND(MAX(v.length),0),
      ROUND(MAX(v.brake_hp),0),
      MAX(v.tonccd_id)
    FROM observer.isvessels v,
      observer.istrips t
    WHERE t.vess_id  =v.vess_id
    AND t.tripcd_id IN (7057, 30)
    GROUP BY v.vessel_name,
      v.cfv
    MINUS
    SELECT VESSEL_NAME,
      VESSEL_CFV,
      COUNTRY_ID,
      LENGTH,
      BRAKE_HP,
      TONC_ID
    FROM temp_tag_vesselcodes
  );
COMMIT;
INSERT
INTO temp_tag_contactcodes
  (
    FIRST_NAME,
    LAST_NAME,
    COMMENTS
  )
  (SELECT trim(SUBSTR(o.observer, instr(o.observer,',')+1)) firstname,
      trim(SUBSTR(o.observer, 1, instr(o.observer,',') -1)) lastname,
      'ISDB ISOBSERVERCODES: '
      ||o.obscd_id
    FROM observer.isobservercodes o,
      observer.istrips t
    WHERE o.obscd_id NOT IN (0, 807, 989)
    AND t.obscd_id        =o.obscd_id
    AND t.tripcd_id      IN (7057, 30)
    GROUP BY o.obscd_id,
      o.observer
    MINUS
    SELECT FIRST_NAME, LAST_NAME, COMMENTS FROM temp_tag_contactcodes
  );
COMMIT;
INSERT INTO temp_tag_nafocodes
  (nafo_abbrev, nafo_description
  )
SELECT nafarea,
  'NAFAREA '
  ||trim(nafarea)
  ||' from ISDB ISFISHSETS'
FROM
  (SELECT upper(trim(nafarea_id)) nafarea
  FROM observer.isfishsets f,
    observer.istrips t
  WHERE f.trip_id  =t.trip_id
  AND t.tripcd_id IN (7057, 30)
  AND nafarea_id  IS NOT NULL
  MINUS
  SELECT upper(trim(nafo_abbrev)) FROM isdb_halibut.temp_tag_nafocodes
  );
COMMIT;


insert into temp_tag_trips (trip_name, vessel_id, setrange, board_date, landing_date, contact_id, datasource_id, datasource_key, comments)
(
SELECT a.trip,
  b.vessel_id,
  a.setrange,
  a.board_date,
  a.landing_date,
  c.contact_id,
  1 datasource_id,
  trip_id datasource_key,
  'Auto trips loaded from isdb where speccd_id=30 and mrphcd_id IN (20, 40) and tripcd_id IN (7057, 30) and board_year >= 2010' comments
FROM
  (SELECT trip,
    trip_id,
    tripcd_id,
    board_date,
    landing_date,
    first_name,
    last_name,
    vessel,
    cfv,
    MIN(set_no)
    ||'-'
    || MAX(set_no) setrange
  FROM
    (SELECT a.*,
      CASE
        WHEN a.mrphcd_id = 20
        THEN
          (SELECT NVL(mv.description,'00')
          FROM observer.ismorphvaluecodes mv,
            observer.isfishmorphs m
          WHERE m.fish_id  =a.fish_id
          AND mv.mrphcd_id =m.mrphcd_id
          AND mv.mrphvcd_id=m.mrphvcd_id
          AND m.mrphcd_id  =100
          )
        WHEN a.mrphcd_id = 49
        THEN
          (SELECT NVL(mv.description,'00')
          FROM observer.ismorphvaluecodes mv,
            observer.isfishmorphs m
          WHERE m.fish_id  =a.fish_id
          AND mv.mrphcd_id =m.mrphcd_id
          AND mv.mrphvcd_id=m.mrphvcd_id
          AND m.mrphcd_id  =101
          )
      END tagprefix
    FROM
      (SELECT t.trip,
        t.trip_id,
        t.tripcd_id,
        t.board_date,
        t.landing_date,
        CASE
          WHEN instr(o.observer,',') > 0
          THEN upper(trim(SUBSTR(o.observer, instr(o.observer,',')+1)))
          ELSE NULL
        END first_name,
        CASE
          WHEN instr(o.observer,',') > 0
          THEN upper(trim(SUBSTR(o.observer, 1, instr(o.observer,',')-1)))
          ELSE upper(trim(o.observer))
        END last_name,
        v.vessel_name vessel,
        v.cfv,
        fs.fishset_id,
        fs.set_no,
        fs.nafarea_id nafarea,
        fs.setcd_id,
        c.catch_id,
        p.setdate,
        p.latitude,
        p.longitude,
        TO_CHAR(p.setdate, 'dd') da,
        TO_CHAR(p.setdate, 'mm') mo,
        TO_CHAR(p.setdate, 'yyyy') yr,
        p.depth_min,
        p.depth_max,
        g.gearcd_id,
        c.speccd_id,
        f.fish_no,
        f.sexcd_id sex,
        f.fish_length LEN,    -- length in cm
        f.fish_weight weight, -- weight from grams
        t.comments tnote,
        fs.comments fsnote,
        f.comments fnote,
        f.last_update_by userid,
        f.last_update_date update_date,
        m.fish_id,
        m.mrphcd_id,
        m.mrphvcd_id,
        m.quant_value tag_id
      FROM OBSERVER.ISVESSELS V,
        OBSERVER.ISTRIPS t,
        OBSERVER.ISFISHSETS fs,
        (SELECT fishset_id,
          MAX(setdate) setdate,
          AVG(latitude) latitude,
          AVG(longitude) longitude,
          MIN(depth) depth_min,
          MAX(depth) depth_max
        FROM
          (SELECT fishset_id,
            setdate,
            latitude,
            longitude,
            depth
          FROM OBSERVER.isstartpnt
          UNION
          SELECT fishset_id, setdate, latitude, longitude, depth FROM OBSERVER.isendpnt
          )
        GROUP BY fishset_id
        ) p,
        OBSERVER.iscatches c,
        OBSERVER.isfish f,
        OBSERVER.isfishmorphs m,
        OBSERVER.isobservercodes o,
        OBSERVER.isgears g
      WHERE v.vess_id    =t.vess_id
      AND v.license_no   =t.license_no
      AND t.trip_id      =fs.trip_id
      AND t.trip_id NOT IN
        (SELECT datasource_key FROM temp_tag_trips WHERE datasource_id=1
        )
      AND t.obscd_id                               =o.obscd_id
      AND fs.fishset_id                            =c.fishset_id
      AND fs.gear_id                               =g.gear_id
      AND p.fishset_id                             =c.fishset_id
      AND c.catch_id                               =f.catch_id
      AND f.fish_id                                =m.fish_id
      AND c.speccd_id                              = 30
      AND t.tripcd_id                             IN (7057, 30)
      AND m.mrphcd_id                             IN (20, 49)
      AND to_number(TO_CHAR(t.board_date,'yyyy')) >= 2010
      ) a
    )
  WHERE tagprefix='ST'
  GROUP BY trip,
    trip_id,
    tripcd_id,
    board_date,
    landing_date,
    first_name,
    last_name,
    vessel,
    cfv
  ) a,
  temp_tag_vesselcodes b,
  temp_tag_contactcodes c
WHERE a.vessel         =b.vessel_name
AND a.cfv              =b.vessel_cfv
AND a.last_name        =c.last_name
AND NVL(a.first_name,0)=NVL(c.first_name,0)

);

commit;


insert into temp_tag_animals (datasource_id, datasource_key, spec_id, sex_id)
(
select 1 datasource_id, 
      'TRIP_ID:'||trip_id||', FISHSET_ID:'||fishset_id||', CATCH_ID:'||catch_id||', FISH_NO:'||fish_no datasource_key, 
      speccd_id, sex 
from
(select a.*, 
case 
when a.mrphcd_id = 20 
then 
(select NVL(mv.description,'00') from observer.ismorphvaluecodes mv, observer.isfishmorphs m where m.fish_id=a.fish_id and mv.mrphcd_id=m.mrphcd_id and mv.mrphvcd_id=m.mrphvcd_id and m.mrphcd_id=100)
when a.mrphcd_id = 49 
then 
(select NVL(mv.description,'00') from observer.ismorphvaluecodes mv, observer.isfishmorphs m where m.fish_id=a.fish_id and mv.mrphcd_id=m.mrphcd_id and mv.mrphvcd_id=m.mrphvcd_id and m.mrphcd_id=101)
end tagprefix
 from  (
select t.trip, t.trip_id, t.tripcd_id, t.board_date, t.landing_date, 
      case when instr(o.observer,',') > 0 then upper(trim(substr(o.observer, instr(o.observer,',')+1))) else null end first_name, 
      case when instr(o.observer,',') > 0 then upper(trim(substr(o.observer, 1, instr(o.observer,',')-1))) else upper(trim(o.observer)) end last_name, 
      v.vessel_name vessel, v.cfv, fs.fishset_id,
     fs.set_no, fs.nafarea_id nafarea, fs.setcd_id, c.catch_id, p.setdate, p.latitude, p.longitude,
     to_char(p.setdate, 'dd') da, to_char(p.setdate, 'mm') mo, to_char(p.setdate, 'yyyy') yr,
     p.depth_min, p.depth_max, g.gearcd_id, c.speccd_id,
    f.fish_no, f.sexcd_id sex, f.fish_length len,  -- length in cm
    f.fish_weight weight, -- weight from grams
    t.comments tnote, fs.comments fsnote, f.comments fnote,
     f.last_update_by userid, f.last_update_date update_date,
     m.fish_id, m.mrphcd_id, m.mrphvcd_id, m.quant_value tag_id
from 
     OBSERVER.ISVESSELS V,
     OBSERVER.ISTRIPS t,
     OBSERVER.ISFISHSETS fs,
     (select fishset_id, max(setdate) setdate, avg(latitude) latitude, 
                avg(longitude) longitude, min(depth) depth_min, max(depth) depth_max
      from ( select fishset_id, setdate, latitude, longitude, depth 
                 from OBSERVER.isstartpnt
	     union 
	     select fishset_id, setdate, latitude, longitude, depth 
                 from OBSERVER.isendpnt) 
      group by fishset_id) p,
      OBSERVER.iscatches c,
      OBSERVER.isfish f,
      OBSERVER.isfishmorphs m,                
      OBSERVER.isobservercodes o,
      OBSERVER.isgears g
where    v.vess_id=t.vess_id
	  and v.license_no=t.license_no
	  and t.trip_id=fs.trip_id
	  and t.obscd_id=o.obscd_id
	  and fs.fishset_id=c.fishset_id
	  and fs.gear_id=g.gear_id
	  and p.fishset_id=c.fishset_id
	  and c.catch_id=f.catch_id
	  and f.fish_id=m.fish_id        
	  and c.speccd_id = 30
	  and t.tripcd_id IN (7057, 30)
	  and m.mrphcd_id in (20, 49)
           and to_number(to_char(t.board_date,'yyyy')) >= 2010
) a)
where tagprefix='ST'
group by trip_id, fishset_id, catch_id, fish_no, speccd_id, sex
minus
select datasource_id, datasource_key, spec_id, sex_id from temp_tag_animals
where datasource_id=1);

commit;


insert into temp_tag_tags (tag_no, tagtype_id, program_id, tagprefix, tagcolour_id, initialdeploy, retention, reward, comments, datasource_id, datasource_key)
(
select trim(to_char(tag_id,'99999')) tag_no, 
       1 tagtype,
       case 
       when yr=2010 then 4 
       when yr=2012 then 7
       when yr=2014 then 19
       when yr=2016 then 21
       when yr=2018 then 22
       when yr=2019 then 23
       when yr=2020 then 28
       else null end progid, 
       'ST' tagprefix, 1 tagcolour_id, board_date intialdeploy, '3-5 years approx. life expectancy' retention,  100 reward,
       'Intialdeploy set to trip boarddate. Auto tags loaded from isdb where speccd_id=30 and mrphcd_id IN (20, 40) and tripcd_id IN (7057, 30) and board_year >= 2010' comments,
       1 datasource_id, 'TRIP_ID:'||trip_id||', FISHSET_ID:'||fishset_id||', CATCH_ID:'||catch_id||', FISH_NO:'||fish_no datasource_key
from
(
select a.*, 
case 
when a.mrphcd_id = 20 
then 
(select NVL(mv.description,'00') from observer.ismorphvaluecodes mv, observer.isfishmorphs m where m.fish_id=a.fish_id and mv.mrphcd_id=m.mrphcd_id and mv.mrphvcd_id=m.mrphvcd_id and m.mrphcd_id=100)
when a.mrphcd_id = 49 
then 
(select NVL(mv.description,'00') from observer.ismorphvaluecodes mv, observer.isfishmorphs m where m.fish_id=a.fish_id and mv.mrphcd_id=m.mrphcd_id and mv.mrphvcd_id=m.mrphvcd_id and m.mrphcd_id=101)
end tagprefix
 from  (
select t.trip, t.trip_id, t.tripcd_id, t.board_date, t.landing_date, o.observer,
      v.vessel_name vessel, v.cfv, fs.fishset_id,
     fs.set_no, fs.nafarea_id nafarea, fs.setcd_id, c.catch_id, p.setdate, p.latitude, p.longitude,
     to_char(p.setdate, 'dd') da, to_char(p.setdate, 'mm') mo, to_char(p.setdate, 'yyyy') yr,
     p.depth_min, p.depth_max, g.gearcd_id, c.speccd_id,
    f.sexcd_id sex, f.fish_length len,  -- length in cm
    f.fish_no, f.fish_weight weight,	-- weight in grams
    t.comments tnote, fs.comments fsnote, f.comments fnote,
     f.last_update_by userid, f.last_update_date update_date,
     m.fish_id, m.mrphcd_id, m.mrphvcd_id, m.quant_value tag_id
from 
     OBSERVER.ISVESSELS V,
     OBSERVER.ISTRIPS t,
     OBSERVER.ISFISHSETS fs,
     (select fishset_id, max(setdate) setdate, avg(latitude) latitude, 
                avg(longitude) longitude, min(depth) depth_min, max(depth) depth_max
      from ( select fishset_id, setdate, latitude, longitude, depth 
                 from OBSERVER.isstartpnt
	     union 
	     select fishset_id, setdate, latitude, longitude, depth 
                 from OBSERVER.isendpnt) 
      group by fishset_id) p,
      OBSERVER.iscatches c,
      OBSERVER.isfish f,
      OBSERVER.isfishmorphs m,                
      OBSERVER.isobservercodes o,
      OBSERVER.isgears g
where    v.vess_id=t.vess_id
	  and v.license_no=t.license_no
	  and t.trip_id=fs.trip_id 
	  and t.obscd_id=o.obscd_id
	  and fs.fishset_id=c.fishset_id
	  and fs.gear_id=g.gear_id
	  and p.fishset_id=c.fishset_id
	  and c.catch_id=f.catch_id
	  and f.fish_id=m.fish_id        
	  and c.speccd_id = 30
	  and t.tripcd_id IN (7057, 30)
	  and m.mrphcd_id in (20, 49)
           and to_number(to_char(t.board_date,'yyyy')) >= 2010
) a) 
where tagprefix='ST'
minus
select tag_no, tagtype_id, program_id, tagprefix, tagcolour_id, initialdeploy, retention, reward, 
'Intialdeploy set to trip boarddate. Auto tags loaded from isdb where speccd_id=30 and mrphcd_id IN (20, 40) and tripcd_id IN (7057, 30) and board_year >= 2010' comments, datasource_id, datasource_key
from temp_tag_tags
where datasource_id=1
);

commit;


insert into temp_tag_events
(tag_id, eventtype_id, animal_id, contact_id, datasource_id, datasource_key, trip_id, set_no, gear_id, 
 year, month, day, time, lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec, min_depth, max_depth, nafo_id,
 length, weight, sex_id, comments)
(
select a.tag_id, a.eventtype_id, a.animal_id, c.contact_id, a.datasource_id, 
      a.datasource_key, tt.trip_id, set_no, a.gear_id,  
      to_number(to_char(tt.board_date,'YYYY')), to_number(a.mo), to_number(a.da), to_number(a.settime), 
      to_number(a.lat_deg), to_number(a.lat_min), to_number(a.lat_sec),
      to_number(a.lon_deg), to_number(a.lon_min), to_number(a.lon_sec), 
      a.depth_min, a.depth_max, 
      nc.nafo_id, a.length, a.weight, a.sex, a.fnote
from temp_tag_contactcodes c, temp_tag_nafocodes nc, temp_tag_trips tt,
(
select t.tag_id, 1 eventtype_id, an.animal_id, 1 datasource_id, 
      'fishset_id:'||a.fishset_id||'catch_id:'||a.catch_id||'fish_no:'||a.fish_no datasource_key, a.set_no, a.gear_id, 
      a.yr, a.mo, a.da, a.settime, trunc(a.lat) lat_deg, 
      trunc(a.lat*60-(trunc(a.lat)*60))+trunc(round(a.lat*3600 - ( (trunc(a.lat)*3600) + (trunc(a.lat*60-(trunc(a.lat)*60))*60) ))/60) lat_min, 
       round(a.lat*3600 - ( (trunc(a.lat)*3600)+(trunc(a.lat*60-(trunc(a.lat)*60))+trunc(round(a.lat*3600 - ( (trunc(a.lat)*3600) + (trunc(a.lat*60-(trunc(a.lat)*60))*60) ))/60))*60))  lat_sec, 
      trunc(a.lon) lon_deg, trunc(a.lon*60-(trunc(a.lon)*60))+trunc(round(a.lon*3600 - ( (trunc(a.lon)*3600) + (trunc(a.lon*60-(trunc(a.lon)*60))*60) ))/60) lon_min, 
       round(a.lon*3600 - ( (trunc(a.lon)*3600)+(trunc(a.lon*60-(trunc(a.lon)*60))+trunc(round(a.lon*3600 - ( (trunc(a.lon)*3600) + (trunc(a.lon*60-(trunc(a.lon)*60))*60) ))/60))*60))  lon_sec, 
      a.depth_min, a.depth_max, a.length, a.weight, a.sex, a.fnote, a.nafarea, a.first_name, a.last_name, a.trip
from temp_tag_tags t, temp_tag_animals an,
(
select trim(to_char(m.quant_value,'99999')) tag_no, --trim(to_char(tag_id,'99999'))
	t.trip, t.trip_id, t.tripcd_id, t.board_date, t.landing_date, o.observer,
      case when instr(o.observer,',') > 0 then upper(trim(substr(o.observer, instr(o.observer,',')+1))) else null end first_name, 
      case when instr(o.observer,',') > 0 then upper(trim(substr(o.observer, 1, instr(o.observer,',')-1))) else upper(trim(o.observer)) end last_name, 
      v.vessel_name vessel, v.cfv, fs.fishset_id, g.gearcd_id gear_id, 
     fs.set_no, fs.nafarea_id nafarea, fs.setcd_id, c.catch_id, 
     p.setdate, p.latitude lat, p.longitude lon, p.depth_min, p.depth_max,
     to_char(p.setdate, 'dd') da, to_char(p.setdate, 'mm') mo, to_char(p.setdate, 'yyyy') yr,
     p.settime,
    f.fish_no, f.sexcd_id sex, f.fish_length length, f.fish_weight weight,
    t.comments tnote, fs.comments fsnote, f.comments fnote,
     f.last_update_by userid, f.last_update_date update_date,
     m.fish_id, m.mrphcd_id, m.mrphvcd_id, m.quant_value,     
	case 
	when m.mrphcd_id = 20 
	then 
	(select NVL(mv.description,'00') from observer.ismorphvaluecodes mv, observer.isfishmorphs m where m.fish_id=f.fish_id and mv.mrphcd_id=m.mrphcd_id and mv.mrphvcd_id=m.mrphvcd_id and m.mrphcd_id=100)
	when m.mrphcd_id = 49 
	then 
	(select NVL(mv.description,'00') from observer.ismorphvaluecodes mv, observer.isfishmorphs m where m.fish_id=f.fish_id and mv.mrphcd_id=m.mrphcd_id and mv.mrphvcd_id=m.mrphvcd_id and m.mrphcd_id=101)
	end tagprefix
from 
     OBSERVER.ISVESSELS V,
     OBSERVER.ISTRIPS t,
     OBSERVER.ISFISHSETS fs,
     (select fishset_id, max(setdate) setdate, max(settime) settime, avg(latitude) latitude, 
                avg(longitude) longitude, min(depth) depth_min, max(depth) depth_max
      from ( select fishset_id,  to_date(to_char(nvl(setdate, to_date('1888-01-01', 'YYYY-MM-DD')))) setdate, settime, latitude, longitude, depth 
                 from OBSERVER.isstartpnt
	     union 
	     select fishset_id, to_date(to_char(nvl(setdate, to_date('1888-01-01', 'YYYY-MM-DD')))) setdate, settime, latitude, longitude, depth 
                 from OBSERVER.isendpnt) 
      group by fishset_id) p,
      OBSERVER.iscatches c,
      OBSERVER.isfish f,
      OBSERVER.isfishmorphs m,                
      OBSERVER.isobservercodes o,
      OBSERVER.isgears g
where    v.vess_id=t.vess_id
	  and v.license_no=t.license_no
	  and t.trip_id=fs.trip_id
	  and t.obscd_id=o.obscd_id
	  and fs.fishset_id=c.fishset_id
	  and fs.gear_id=g.gear_id
	  and p.fishset_id=c.fishset_id
	  and c.catch_id=f.catch_id
	  and f.fish_id=m.fish_id        
	  and c.speccd_id = 30
	  and t.tripcd_id IN (7057, 30)
	  and m.mrphcd_id in (20, 49)
          and to_number(to_char(t.board_date,'yyyy')) >= 2010
) a    
where a.tagprefix='ST' 
      AND a.tag_no=trim(t.tag_no(+))  
      AND a.tagprefix=t.tagprefix
      AND CASE when instr(an.datasource_key,'TRIP_ID:')>0 then SUBSTR(an.datasource_key, instr(an.datasource_key,'TRIP_ID:')   +8, instr( SUBSTR(an.datasource_key,instr(an.datasource_key,'TRIP_ID:')+8),',')-1) else null end = a.trip_id
      AND CASE when instr(an.datasource_key,'FISHSET_ID:')>0 then SUBSTR(an.datasource_key, instr(an.datasource_key,'FISHSET_ID:')+11, instr(SUBSTR(an.datasource_key, instr(an.datasource_key,'FISHSET_ID:')+11),',')-1) else null end = a.fishset_id
      AND CASE when instr(an.datasource_key,'CATCH_ID:')>0 then SUBSTR(an.datasource_key, instr(an.datasource_key,'CATCH_ID:')  +9, instr(SUBSTR(an.datasource_key, instr(an.datasource_key,'CATCH_ID:')+9),',')-1) else null end = a.catch_id
      AND CASE when instr(an.datasource_key,'FISH_NO')>0 then SUBSTR(an.datasource_key, instr(an.datasource_key,'FISH_NO:')   +8) else null end  = a.fish_no
) a
where a.last_name=c.last_name 
and nvl(a.first_name,0)=nvl(c.first_name,0) 
and trim(upper(a.nafarea))=trim(upper(nc.nafo_abbrev))
and trim(a.trip) = trim(tt.trip_name)
minus
select tag_id, eventtype_id, animal_id, contact_id, datasource_id, datasource_key, trip_id, set_no, gear_id,
 year, month, day, time, lat_deg, lat_min, lat_sec, 
 lon_deg, lon_min, lon_sec, min_depth, max_depth, nafo_id, length, weight, sex_id, comments from temp_tag_events where datasource_id=1
) ;

commit;

