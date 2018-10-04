SET search_path TO tender_development;

CREATE TABLE zindex_indicator (
    id character varying(255) PRIMARY KEY,
    data jsonb,
    created timestamp without time zone,
    createdby character varying(255),
    createdbyversion character varying(255),
    modified timestamp without time zone,
    modifiedby character varying(255),
    modifiedbyversion character varying(255)
);

CREATE INDEX zindex_indicator_createdby_idx ON zindex_indicator (createdby);
CREATE INDEX zindex_indicator_modified_idx ON zindex_indicator (modified);
CREATE INDEX zindex_indicator_modifiedby_idx ON zindex_indicator (modifiedby);
CREATE INDEX zindex_indicator_modifiedbyversion_idx ON zindex_indicator (modifiedbyversion);
CREATE INDEX zindex_indicator_data_idx ON zindex_indicator USING gin (data jsonb_path_ops);
CREATE INDEX zindex_indicator_bodyid_idx ON zindex_indicator (createdby, (data->>'bodyId'));


CREATE TABLE zindex_offense (
    organizationid character varying(8) NOT NULL,
    year integer NOT NULL,
    subject character varying(255) NOT NULL,
    isseriousoffense boolean,
    isuohsconfirmed boolean,
	url text
);
CREATE INDEX zindex_offense_organization_id_idx ON zindex_offense (organizationid);
CREATE INDEX zindex_offense_year_idx ON zindex_offense (year);


INSERT INTO zindex_offense values ('231223',2016,'Výkon technického dozoru investora stavby Víceúčelové sportovní centrum Na Chobotě""',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14452.html');
INSERT INTO zindex_offense values ('231223',2017,'MŠ Laudova 1030/3, Praha 6 – Řepy, rekonstrukce osvětlení',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14818.html');
INSERT INTO zindex_offense values ('231223',2017,'ZŠ genpor. Fr. Peřiny, Socháňova 1139, Praha 6 – Řepy – rekonstrukce osvětlení',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14814.html');
INSERT INTO zindex_offense values ('231312',2017,'Vybudování energetického managementu s automatickým zápisem dat',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15328.html');
INSERT INTO zindex_offense values ('234516',2015,'Stabilizace a posílení ekologických funkcí sídelní zeleně ve vybraných lokalitách města Kladna – Park Lesík',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12502.html');
INSERT INTO zindex_offense values ('235873',2015,'Výběr provozovatele vodohospodářského majetku obce Velký Osek',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14040.html');
INSERT INTO zindex_offense values ('236021',2016,'Opatření ke snížení energetické náročnosti veřejného osvětlení města Čáslav',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14202.html');
INSERT INTO zindex_offense values ('236021',2017,'Obnova veřejné zeleně v Čáslavi',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15442.html');
INSERT INTO zindex_offense values ('236021',2017,'Obnova veřejné zeleně v Čáslavi',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15381.html');
INSERT INTO zindex_offense values ('236195',2015,'Výběr dodavatele sdružených služeb dodávek zemního plynu na rok 2016',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13156.html');
INSERT INTO zindex_offense values ('236195',2015,'Výběr dodavatele sdružených služeb dodávek zemního plynu na roky 2016 a 2017',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13154.html');
INSERT INTO zindex_offense values ('236977',2015,'Rekonstrukce rozvodů elektro v ZŠ V. Havla, Kralupy nad Vltavou',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12756.html');
INSERT INTO zindex_offense values ('238295',2017,'Svoz a likvidace odpadu na území Statutárního města Mladá Boleslav',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14867.html');
INSERT INTO zindex_offense values ('239640',2018,'Stavební úpravy sportovní haly BIOS Poděbrady',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15366.html');
INSERT INTO zindex_offense values ('240079',2016,'Dodávka tří zařízení pro obousměrné úsekové měření rychlosti vozidel',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14819.html');
INSERT INTO zindex_offense values ('240079',2017,'Dodávka tří zařízení pro obousměrné úsekové měření rychlosti vozidel',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15260.html');
INSERT INTO zindex_offense values ('240192',2015,'Plynová kotelna a ústřední topení - bytové domy Jívanská 1745 – 1746, Praha 9 – Horní Počernice',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12563.html');
INSERT INTO zindex_offense values ('240702',2016,'Projektová dokumentace – Objekt základní školy na Komenského náměstí v Říčanech',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14674.html');
INSERT INTO zindex_offense values ('241202',2015,'Tělocvična, ZŠ Dolní Břežany',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13872.html');
INSERT INTO zindex_offense values ('241229',2015,'Obec Horoměřice - dostavba objektů školských zařízení – II. etapa',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13575.html');
INSERT INTO zindex_offense values ('241318',2016,'Nová základní škola v Jesenici – projektová dokumentace',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13927.html');
INSERT INTO zindex_offense values ('241687',2015,'Oprava oplocení objektu ZŠ prof. Otokara Chlupa',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13852.html');
INSERT INTO zindex_offense values ('243132',2015,'Sdružené služby dodávek elektrické energie pro město Příbram',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13570.html');
INSERT INTO zindex_offense values ('243132',2015,'Sdružené služby dodávek zemního plynu pro město Příbram',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13545.html');
INSERT INTO zindex_offense values ('243272',2017,'Modernizace přestupního terminálu Sedlčany',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15289.html');
INSERT INTO zindex_offense values ('244155',2017,'Nové Strašecí – dostavba kanalizace a ČOV',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15272.html');
INSERT INTO zindex_offense values ('244732',2015,'REKONSTRUKCE VYTÁPĚNÍ ZÁKLADNÍ ŠKOLY GRÜNWALDOVA',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13210.html');
INSERT INTO zindex_offense values ('245836',2015,'Konsolidace IT infrastruktury a rozvoj služeb TC ORP Český Krumlov - II',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13437.html');
INSERT INTO zindex_offense values ('245941',2015,'Snížení energetické náročnosti areálu ZŠ Školní, Kaplice',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12740.html');
INSERT INTO zindex_offense values ('247618',2015,'Osobní automobily pro město Třeboň',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13473.html');
INSERT INTO zindex_offense values ('253081',2016,'Rekonstrukce zázemí sportovně relaxačního areálu Veselí nad Lužnicí',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13648.html');
INSERT INTO zindex_offense values ('254657',2015,'Magistrát města Karlovy Vary – dodávka telekomunikačních služeb',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13675.html');
INSERT INTO zindex_offense values ('254843',2015,'Správa bytového a nebytového majetku ve vlastnictví města Ostrov',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12640.html');
INSERT INTO zindex_offense values ('254843',2017,'Smlouvy o závazku veřejné služby ve veřejné linkové osobní dopravě',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15132.html');
INSERT INTO zindex_offense values ('255661',2016,'Rekonstrukce veřejného osvětlení v Klatovech',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14407.html');
INSERT INTO zindex_offense values ('255661',2016,'Rekonstrukce veřejného osvětlení v Klatovech',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15087.html');
INSERT INTO zindex_offense values ('259047',2016,'Rokycany - V. Nového prodloužení místní komunikace',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15071.html');
INSERT INTO zindex_offense values ('259047',2016,'Zatraktivnění plaveckého bazénu – úpravy objektu krytého bazénu',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14249.html');
INSERT INTO zindex_offense values ('259047',2017,'Dodávka a montáž 3 rotátorů s výsuvnými zásuvkami',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15313.html');
INSERT INTO zindex_offense values ('259438',2015,'Dětská hřiště a zahrady v přírodním stylu ve městě Kraslice',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13820.html');
INSERT INTO zindex_offense values ('259438',2015,'Pojištění majetku a odpovědnosti Kraslice 2015',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13694.html');
INSERT INTO zindex_offense values ('260746',2017,'Správa a údržba veřejného osvětlení v Mimoni na rok 2017 – 2021',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14881.html');
INSERT INTO zindex_offense values ('260771',2016,'Konsolidace IT infrastruktury a nové služby TC ORP Nový Bor',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13613.html');
INSERT INTO zindex_offense values ('261602',2015,'Údržba a ošetřování vybrané veřejné zeleně ve městě Rumburk',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12835.html');
INSERT INTO zindex_offense values ('261891',2015,'Bourací práce KaSS',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13907.html');
INSERT INTO zindex_offense values ('261891',2015,'Skládka Chomutov – rekultivace a odplynění',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12549.html');
INSERT INTO zindex_offense values ('261912',2016,'Chytré hřiště v Kadani',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14567.html');
INSERT INTO zindex_offense values ('261912',2016,'Chytré hřiště v Kadani',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14653.html');
INSERT INTO zindex_offense values ('262978',2016,'Divadlo F. X. ŠALDY – výměna zařízení tyristorovny',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13806.html');
INSERT INTO zindex_offense values ('262978',2017,'GDPR – realizace části 1. etapy Koncepce GDPR SML',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15239.html');
INSERT INTO zindex_offense values ('262978',2017,'Mateřská škola Nová Ruda / Vratislavice n. N.',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14753.html');
INSERT INTO zindex_offense values ('265209',2016,'ZŠ J. A. Komenského – výměna kotlů a rozvodů v kotelně',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14589.html');
INSERT INTO zindex_offense values ('265209',2017,'Dlouhodobé zajištění dopravní obslužnosti územního obvodu města Louny veřejnými službami v přepravě cestujících II',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14802.html');
INSERT INTO zindex_offense values ('265781',2016,'Výměna rozvodů vody a kanalizace, ZŠ Jižní čp. 2777, Žatec',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14409.html');
INSERT INTO zindex_offense values ('266027',2015,'Revitalizace MŠ Paraplíčko v Litvínově 2015',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14125.html');
INSERT INTO zindex_offense values ('266621',2015,'Údržba městské zeleně na území Statutárního města Teplice v letech 2016-2017',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14003.html');
INSERT INTO zindex_offense values ('266621',2015,'Údržba městské zeleně na území Statutárního města Teplice v letech 2016-2017',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13027.html');
INSERT INTO zindex_offense values ('266621',2015,'Údržba městské zeleně na území Statutárního města Teplice v letech 2016-2017',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13421.html');
INSERT INTO zindex_offense values ('266621',2017,'Nové pracoviště elektrodispečinku včetně dálkového ovládání měníren pro trolejbusy v Teplicích',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15434.html');
INSERT INTO zindex_offense values ('267449',2015,'Dodávka elektrické energie formou centrálního zadávání pro Město Havlíčkův Brod a jím zřizované příspěvkové organizace města a obchodní organizace s majetkovým vkladem města v období od 1. 4. 2015 do 31. 3. 2016',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14375.html');
INSERT INTO zindex_offense values ('268810',2015,'Parkovací dům pro kola v lokalitě Univerzity Hradec Králové a Fakultní nemocnice Hradec Králové',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13444.html');
INSERT INTO zindex_offense values ('268810',2015,'Poskytovatel telekomunikačních služeb pro statutární město Hradec Králové – mobilní telefonie',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15441.html');
INSERT INTO zindex_offense values ('268810',2016,'Fotbalový stadion Hradec Králové - DUR,DSP a DPS',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13915.html');
INSERT INTO zindex_offense values ('268810',2017,'Portál občana a komunikace s úřadem přes internet',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15300.html');
INSERT INTO zindex_offense values ('269719',2016,'Správa veřejného osvětlení v Třebechovicích p. O.',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13863.html');
INSERT INTO zindex_offense values ('269719',2016,'Správa veřejného osvětlení v Třebechovicích p.O.',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13888.html');
INSERT INTO zindex_offense values ('270211',2016,'Nákup datového uložiště a související služby – opakované řízení',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14451.html');
INSERT INTO zindex_offense values ('271632',2015,'Areál ZŠ Soudná č. p. 15 – vybavení objektu denního stacionáře kolejnicový systém a rehabilitační zařízení" opakované řízení a Areál ZŠ Soudná č. p. 15 – vybavení objektu denního stacionáře ostatní vybavení"',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14283.html');
INSERT INTO zindex_offense values ('271632',2015,'Areál ZŠ Soudná č.p. 15 – vybavení objektu denního stacionáře kolejnicový systém a rehabilitační zařízení, opakované řízení',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13905.html');
INSERT INTO zindex_offense values ('272523',2015,'Projektová dokumentace stavby – Stavební úpravy budovy čp. 104 „Café Herzog“ Broumov',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13250.html');
INSERT INTO zindex_offense values ('272566',2016,'Červený Kostelec, rekonstrukce 4 mostů',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13822.html');
INSERT INTO zindex_offense values ('272680',2017,'AQUA MINERALIS GLACENSIS – Revitalizace parku A. Jiráska – Hronov',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14962.html');
INSERT INTO zindex_offense values ('272728',2016,'Provozovatel veřejného a slavnostního osvětlení na území Města Jaroměř',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14717.html');
INSERT INTO zindex_offense values ('274046',2015,'Labská hotelová škola - výměna oken',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13425.html');
INSERT INTO zindex_offense values ('275301',2015,'Nakládání s komunálním odpadem ve Městě Rokytnice v Orlických horách',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13553.html');
INSERT INTO zindex_offense values ('276944',2016,'Pořízení rolby na úpravu ledové plochy v Litomyšli',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14417.html');
INSERT INTO zindex_offense values ('276944',2016,'Pořízení rolby na úpravu ledové plochy v Litomyšli',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14621.html');
INSERT INTO zindex_offense values ('277819',2015,'Čistírna odpadních vod ve Dvoře Králové nad Labem a rekonstrukce kanalizace',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12350.html');
INSERT INTO zindex_offense values ('279676',2015,'Poskytování telekomunikačních služeb pro Město Ústí nad Orlicí a jím založené a zřízené organizace',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12828.html');
INSERT INTO zindex_offense values ('283061',2013,'Nakládání s komunálním odpadem pro město Břeclav',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12916.html');
INSERT INTO zindex_offense values ('283924',2015,'Rekonstrukce MK - ulice Slovenská a Lesní čtvrť I a výstavba opěrné stěny u RD č. p. 2353',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12793.html');
INSERT INTO zindex_offense values ('283924',2015,'Rekonstrukce ul. Svatopluka Čecha, Zlín – Podhoří',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12776.html');
INSERT INTO zindex_offense values ('283924',2016,'Kanalizace Klečůvka',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14301.html');
INSERT INTO zindex_offense values ('283924',2016,'Kanalizace Klečůvka',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14186.html');
INSERT INTO zindex_offense values ('284611',2015,'Digitální úřad nové generace města Valašské Klobouky',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13042.html');
INSERT INTO zindex_offense values ('284891',2015,'B.j.21-PČB-Hodonín, Žižkova.ul',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14169.html');
INSERT INTO zindex_offense values ('284891',2017,'Zimní stadion – úprava vzduchotechniky – odvlhčovací jednotka a protipožární nátěr, Rekonstrukce vzduchotechniky školní kuchyně ZŠ Vančurova Hodonín a Rekonstrukce vzduchotechniky školní kuchyně MŠ Družstevní Hodonín',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14757.html');
INSERT INTO zindex_offense values ('286010',2015,'Integrace informačních systémů statutárního města Jihlavy',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13588.html');
INSERT INTO zindex_offense values ('287351',2016,'Územní plán Kroměříž',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15389.html');
INSERT INTO zindex_offense values ('287351',2017,'Územní plán Kroměříž',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14673.html');
INSERT INTO zindex_offense values ('290629',2016,'Výběr dopravce pro uzavření smlouvy o poskytování veřejných služeb v přepravě cestujících veřejnou linkovou autobusovou dopravou k zajištění dopravní obslužnosti města Třebíč a vybraných okolních obcí',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14712.html');
INSERT INTO zindex_offense values ('290629',2017,'Čištění a zimní údržba místních komunikací a ostatních ploch v majetku města Třebíče',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15285.html');
INSERT INTO zindex_offense values ('292427',2017,'REGENERACE – ÚPRAVY SÍDLIŠTĚ SMETANOVO NÁBŘEŽÍ – V. ETAPA',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14815.html');
INSERT INTO zindex_offense values ('293199',2015,'Smlouva o poradenství a správě odběrných míst a odběrných zařízení při spotřebě elektrické energie a zemního plynu',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14037.html');
INSERT INTO zindex_offense values ('296007',2016,'Nakládání s odpady ve městě Horní Benešov',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14492.html');
INSERT INTO zindex_offense values ('296139',2015,'Dodávka a implementace komponent hardware a software 2',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12542.html');
INSERT INTO zindex_offense values ('297313',2015,'Správa a údržba pohřebišť města Třince 2016-2017',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13349.html');
INSERT INTO zindex_offense values ('297313',2015,'Správa pohřebišť 2016-2017, Údržba pohřebišť 2016-2017',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13330.html');
INSERT INTO zindex_offense values ('297313',2017,'Nakládání s komunálním odpadem ve městě Třinci',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15411.html');
INSERT INTO zindex_offense values ('297313',2017,'Sběrný dvůr Třinec',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15397.html');
INSERT INTO zindex_offense values ('297313',2017,'Údržba komunikací ve vlastnictví města Třince',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14832.html');
INSERT INTO zindex_offense values ('297313',2017,'Údržba komunikací ve vlastnictví města Třince',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15081.html');
INSERT INTO zindex_offense values ('297437',2015,'Nakládání s komunálním odpadem ve Městě Český Těšín II',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14500.html');
INSERT INTO zindex_offense values ('297437',2017,'Komplexní údržba zeleně ve městě Český Těšín',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15036.html');
INSERT INTO zindex_offense values ('297437',2017,'Komplexní údržba zeleně ve městě Český Těšín',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15220.html');
INSERT INTO zindex_offense values ('297569',2016,'Opatření ke snížení energetické náročnosti veřejného osvětlení v Bohumíně',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14144.html');
INSERT INTO zindex_offense values ('299308',2015,'Plán udržitelné městské mobility Olomouc',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14668.html');
INSERT INTO zindex_offense values ('299308',2017,'Dodávka křesel pro Moravské divadlo Olomouc',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14967.html');
INSERT INTO zindex_offense values ('299308',2017,'Protipovodňová opatření III. etapa',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15359.html');
INSERT INTO zindex_offense values ('299529',2015,'Zajištění služeb v oblasti kultury pro město Šternberk na období 2016 – 2020',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13738.html');
INSERT INTO zindex_offense values ('299651',2015,'Centrem Velké Bystřice čistou nohou – IV. etapa',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13786.html');
INSERT INTO zindex_offense values ('301493',2016,'Výstavba sportovní haly, Lipník nad Bečvou',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14017.html');
INSERT INTO zindex_offense values ('44992785',2015,'Brno, Francouzská - rekonstrukce kanalizace a vodovodu',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12898.html');
INSERT INTO zindex_offense values ('44992785',2015,'Dopravní a informační centrum Brno – 2. etapa',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13157.html');
INSERT INTO zindex_offense values ('44992785',2015,'Rekonstrukce střech na bytových domech ulice Houbalova 1-11 lichá čísla, Brno-Líšeň',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13343.html');
INSERT INTO zindex_offense values ('44992785',2015,'Revitalizace sportovních ploch v MČ Brno – Židenice',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12600.html');
INSERT INTO zindex_offense values ('44992785',2015,'Údržba čistoty a zimní údržba místních komunikací správního územní městských částí, opravy, zateplení a výměna oken MŠ,',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12817.html');
INSERT INTO zindex_offense values ('44992785',2015,'Úklid bytových domů',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13200.html');
INSERT INTO zindex_offense values ('44992785',2016,'Administrace výběrových řízení',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14501.html');
INSERT INTO zindex_offense values ('44992785',2016,'Administrace výběrových řízení',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13622.html');
INSERT INTO zindex_offense values ('44992785',2016,'Brno, MČ Chrlice - ul. Rebešovická, dostavba splaškové kanalizace',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14464.html');
INSERT INTO zindex_offense values ('44992785',2016,'Odhlučnění TT Cejl – Zábrdovická včetně bezbariérových zastávek Vojenská nemocnice',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15408.html');
INSERT INTO zindex_offense values ('44992785',2016,'OPRAVY A ÚDRŽBA',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14264.html');
INSERT INTO zindex_offense values ('44992785',2017,'Dostavba kanalizace v Brně II – odborné poradenství',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15377.html');
INSERT INTO zindex_offense values ('44992785',2017,'Implementace autentifikace a enkrypce do systému TETRA',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14885.html');
INSERT INTO zindex_offense values ('44992785',2017,'Modulární Mateřská škola v areálu ZŠ a MŠ Pastviny, Brno-Komín',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14871.html');
INSERT INTO zindex_offense values ('573213',2015,'Dokončení I. etapy společenského objektu Dalovice',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14212.html');
INSERT INTO zindex_offense values ('63410',2016,'Komplexní provoz a podpora ICT ÚMČ Praha 1',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13561.html');
INSERT INTO zindex_offense values ('63461',2017,'Energetické služby a dodávky',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14851.html');
INSERT INTO zindex_offense values ('63517',2015,'Úprava okolí areálu biatlonu v Parku Parukářka a Vybudování parkového vodovodu, grilovacích míst, psího hřiště a úprava dětského hřiště v parku Parukářka',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13760.html');
INSERT INTO zindex_offense values ('63517',2017,'Úklidové služby pro Úřad městské části Praha 3',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15409.html');
INSERT INTO zindex_offense values ('63584',2016,'Zajištění externího správce, tj. outsourcing informačních technologií a služeb',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13957.html');
INSERT INTO zindex_offense values ('63631',2017,'Úklid a údržba zeleně na Praze 5',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14774.html');
INSERT INTO zindex_offense values ('63797',2015,'Zajištění celoroční údržby zeleně a dalších ploch',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13617.html');
INSERT INTO zindex_offense values ('63941',2015,'Správa domovního fondu svěřeného m. č. Praha 10',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14870.html');
INSERT INTO zindex_offense values ('64581',2015,'Dodávky výstrojního materiálu pro Městskou policii hl. m. Prahy',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14333.html');
INSERT INTO zindex_offense values ('64581',2015,'Dodávky výstrojního materiálu pro Městskou policii hl. m. Prahy',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14540.html');
INSERT INTO zindex_offense values ('64581',2015,'Rekonstrukce a dostavba Průmyslového paláce v areálu Výstaviště, Praha 7 – projektové práce',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12555.html');
INSERT INTO zindex_offense values ('64581',2015,'Soustava zadávacích postupů pro zadávání veřejných zakázek',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13680.html');
INSERT INTO zindex_offense values ('64581',2015,'Úklid objektů užívaných Městskou policií hl. m. Prahy',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13411.html');
INSERT INTO zindex_offense values ('64581',2016,'Provozní podpora Informačního systému krizového řízení',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14925.html');
INSERT INTO zindex_offense values ('64581',2016,'Provozní podpora Informačního systému krizového řízení',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14008.html');
INSERT INTO zindex_offense values ('64581',2016,'Servis a údržba vozidel vozového parku Městské policie hl. m. Prahy',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14496.html');
INSERT INTO zindex_offense values ('64581',2016,'Stavba č. 42932 P + R Černý Most III. – inženýrská činnost',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14807.html');
INSERT INTO zindex_offense values ('64581',2016,'Zajištění kritických činností nutných k zajištění fungování osvětlení v hl. m. Praze po 1. 1. 2017',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14873.html');
INSERT INTO zindex_offense values ('64581',2016,'Zajištění správy, provozu a údržby veřejného osvětlení a souvisejících zařízení na území hlavního města Prahy',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14831.html');
INSERT INTO zindex_offense values ('64581',2016,'Zákaz plnění příkazní smlouvy č. PRK/83/04/026346/2016',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14922.html');
INSERT INTO zindex_offense values ('64581',2017,'Dodávka elektřiny a zemního plynu pro veřejné osvětlení na území hlavního města Prahy pro roky 2017 a 2018',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14722.html');
INSERT INTO zindex_offense values ('64581',2017,'Dodávka kamerového systému pro zabezpečení objektu užívaného Městskou policií hl. m. Prahy',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15193.html');
INSERT INTO zindex_offense values ('64581',2017,'Dodávka kamerového systému pro zabezpečení objektu užívaného Městskou policií hl. m. Prahy',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15271.html');
INSERT INTO zindex_offense values ('64581',2017,'Rámcová smlouva o správě, provozu a údržbě veřejného osvětlení a dalších souvisejících zařízení hl. m. Prahy',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15379.html');
INSERT INTO zindex_offense values ('64581',2017,'Zajištění správy, provozu a údržby veřejného osvětlení a dalších souvisejících zařízení hl. m. Prahy',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15122.html');
INSERT INTO zindex_offense values ('64581',2017,'Zajištění správy, provozu a údržby veřejného osvětlení a souvisejících zařízení na území hlavního města Prahy',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14805.html');
INSERT INTO zindex_offense values ('75370',2015,'Náhradní hřiště pro TJ Slovan Spoje Plzeň',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12927.html');
INSERT INTO zindex_offense values ('75370',2015,'Výměna řadičů SSZ – K202, K315, K316, K320 a K620',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14303.html');
INSERT INTO zindex_offense values ('75370',2016,'Čištění mostů, lávek a podchodů a jejich odvodňovačů na místních komunikacích na území města Plzně',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15046.html');
INSERT INTO zindex_offense values ('75370',2016,'Rekonstrukce kanalizace včetně odboček, Plzeň-Litice, Spádná ulice',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13833.html');
INSERT INTO zindex_offense values ('75370',2016,'Údržba Automatizovaného systému řízení dopravy (ASŘD) v Plzni',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14770.html');
INSERT INTO zindex_offense values ('75370',2016,'Údržba Městského kamerového systému (MKS) v Plzni a Proměnného dopravního značení (PDZ) pro parkovací domy Rychtářka a Nové Divadlo v Plzni',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14843.html');
INSERT INTO zindex_offense values ('75370',2017,'Zabezpečení péče o psy a kočky nalezené na území města Plzně',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15032.html');
INSERT INTO zindex_offense values ('75370',2017,'Zabezpečení péče o psy a kočky nalezené na území města Plzně',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15275.html');
INSERT INTO zindex_offense values ('81531',2015,'Elektronický odbavovací systém pro cestující',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13975.html');
INSERT INTO zindex_offense values ('81531',2015,'Správa a údržba zeleně v Městských sadech v Ústí nad Labem',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13855.html');
INSERT INTO zindex_offense values ('81531',2016,'Správa a údržba Městských sadů v Ústí nad Labem',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13871.html');
INSERT INTO zindex_offense values ('845451',2016,'Audit hospodaření',FALSE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13502.html');
INSERT INTO zindex_offense values ('845451',2016,'Systém sdružených nákupů statutárního města Ostrava - JŘSU',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14791.html');
INSERT INTO zindex_offense values ('845451',2016,'Veřejná zakázka na poskytování mobilních telekomunikačních služeb pro statutární město Ostrava, právnické osoby zřízené a založené statutárním městem Ostrava, popřípadě městskými obvody',FALSE,FALSE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14896.html');INSERT INTO zindex_offense values ('845451',2017,'Ekologizace veřejné dopravy Ostrava - Poruba (DÚR a IČ)',TRUE,TRUE,'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15440.html');

update zindex_offense set  organizationid =  lpad(organizationid,8,'0');

# temporarily deleted because it's related to city neighbourhoods
delete from zindex_offense where url in (
'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14753.html',
'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13200.html',
'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12817.html',
'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-12600.html',
'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14264.html',
'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13961.html',
'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13809.html',
'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-13793.html',
'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-15345.html',
'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14285.html',
'http://www.uohs.cz/cs/verejne-zakazky/sbirky-rozhodnuti/detail-14709.html')