package eu.digiwhist.worker.hu.parsed;

import eu.digiwhist.dataaccess.dto.codetables.PublicationSources;
import eu.digiwhist.worker.parser.BaseDigiwhistTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import java.io.IOException;

import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

/**
 * Tender parsed for Hungary old data.
 *
 * @author Marek Mikes
 */
public class HungaryOldDataTenderParser extends BaseDigiwhistTenderParser {
    private static final String VERSION = "1";

    /**
     * Column mapping.
     *
     * <i>corrections:</i>
     * <ul>
     *    <li>column <b>ajk_ksh</b> renamed to <b>eh_ajk_ksh</b> for
     *    {@link #parseBody(org.apache.commons.csv.CSVRecord, java.lang.String)} purposes.</li>
     * </ul>
     */
    private static final String CSV_HEADER = "eh_ev\teh_er_id\teh_item_id\teh_ert_szam\teh_iktsz\teh_url\teh_url_uj"
        + "\teh_rtart\teh_kozzt\teh_hirtip\teh_eljfaj\teh_nuts\teh_nuts_num\teh_nuts1_fv\teh_nuts2_fv\teh_nuts3_fv"
        + "\teh_eu\teh_eu_sz\teh_hszam\teh_hszam_tobb\teh_earlejtes\teh_earlejtes_fv\teh_eredmenyt_indok"
        + "\teh_van_eredmenyt_indok\teh_van_nev\teh_van_dt\teh_van_s\teh_ajk_nev\teh_ajk_cim\teh_ajk_irsz"
        + "\teh_ajk_telep\teh_ajk_tkod\teh_ajk_tel\teh_ajk_fax\teh_ajk_email\teh_ajk_url\teh_ajk_van_email"
        + "\teh_ajk_van_url\tajk_pir\teh_ajk_ksh\teh_ajk_tip\teh_ajk_tev\teh_ny_nev\teh_ny_cim\teh_ny_irsz\teh_ny_telep"
        + "\teh_ny_tkod\teh_ny_orsz\teh_ny_orsz_fv\teh_ny_tel\teh_ny_fax\teh_ny_email\teh_ny_url\teh_ny_van_email"
        + "\teh_ny_van_url\teh_ny_ksh\teh_ny_konz\teh_ny_bid\teh_ny_tip\teh_szszam\teh_sznev\teh_szdt\teh_aj_szam"
        + "\teh_aj_szam_fv\teh_b_s0\teh_b_s1\teh_b_s2\teh_b_s3\teh_b_s_fv\teh_b_s_nhuf\teh_b_s_penzn0\teh_b_s_penzn1"
        + "\teh_b_s_penzn_fv\teh_b_s_afa0\teh_b_s_afa1\teh_b_s_afa_fv\teh_b_s_afasz_e0\teh_b_s_afasz_e1"
        + "\teh_b_s_afasz_e_fv\teh_b_s_afasz_t_fv\teh_b_s_afasz_fv\teh_sz_s0\teh_sz_s1\teh_sz_s2\teh_sz_s3"
        + "\teh_sz_s_fv\teh_sz_s_nhuf\teh_sz_s_penzn0\teh_sz_s_penzn1\teh_sz_s_penzn_fv\teh_sz_s_afa0\teh_sz_s_afa1"
        + "\teh_sz_s_afa_fv\teh_sz_s_afasz_e0\teh_sz_s_afasz_e1\teh_sz_s_afasz_e_fv\teh_sz_s_afasz_t_fv"
        + "\teh_sz_s_afasz_fv\teh_minmax_mins0\teh_minmax_mins1\teh_minmax_mins2\teh_minmax_mins3\teh_minmax_mins_fv"
        + "\teh_minmax_mins_nhuf\teh_minmax_maxs0\teh_minmax_maxs1\teh_minmax_maxs2\teh_minmax_maxs3\teh_minmax_maxs_fv"
        + "\teh_minmax_maxs_nhuf\teh_minmax_penzn0\teh_minmax_penzn1\teh_minmax_penzn_fv\teh_minmax_afa0"
        + "\teh_minmax_afa1\teh_minmax_afa_fv\teh_minmax_afasz_e0\teh_minmax_afasz_e1\teh_minmax_afasz_e_fv"
        + "\teh_minmax_afasz_t_fv\teh_minmax_afasz_fv\teh_k_s0\teh_k_s1\teh_k_s2\teh_k_s_fv\teh_k_s_nhuf\teh_k_s_penzn0"
        + "\teh_k_s_penzn1\teh_k_s_penzn_fv\teh_k_s_afa0\teh_k_s_afa1\teh_k_s_afa_fv\teh_k_s_afasz0\teh_k_s_afasz1"
        + "\teh_k_s_afasz_fv\teh_k_s_hea0\teh_k_s_hea1\teh_k_s_hea_fv\teh_k_s_heasz_fv\teh_sz_e\teh_sz_e_fv\teh_sz_h"
        + "\teh_sz_h_fv\teh_sz_hatlan\teh_sz_hatlan_fv\teh_err_s_ido\teh_alvall\teh_alvall_fv\teh_alvall_s"
        + "\teh_alvall_penzn\teh_alvall_aranysz_e\teh_alvall_aranysz_t\teh_alvall_nemism1\teh_tobbvaltozatu"
        + "\teh_szakasz_eredm\teh_szakasz_eredm_indok\teh_minmax_elt_err\teh_min_s_elt_err\teh_s_max_elt_err"
        + "\teh_b_egysa\teh_s_egysa\teh_mins_egysa\teh_maxs_egysa\teh_k_s_egysa\teh_b_interv\teh_sz_interv\teh_k_interv"
        + "\teh_mins_interv\teh_maxs_interv\teh_b_tartk\teh_sz_tartk\teh_k_tartk\teh_mins_tartk\teh_maxs_tartk"
        + "\teh_cpv_fsz_ft\teh_cpv_ksz_ft\teh_cpv_fsz_ft_num\teh_cpv_ksz_ft_num\teh_cpv_fsz_tt_num\teh_cpv_ksz_tt_num"
        + "\teh_bsz_la\teh_bsz_le\teh_bsz_e\teh_bsz_sz\teh_korr\teh_bsz_szoh\teh_bsz_szsdb\teh_bsz_arsn\teh_keret"
        + "\teh_targy\telj_id\taf_db\taf_tip1\taf_id1\taf_url1\taf_iktsz1\taf_korr_nr1\taf_hatido1\taf_elozm1"
        + "\taf_dok_hatido1\taf_dok_fiz1\taf_dok_ar01\taf_dok_ar11\taf_dok_penzn01\taf_dok_penzn11\taf_felt31_l1"
        + "\taf_felt32_l1\taf_tov_l1\taf_kozzt1\taf_szido_ho1\taf_szido_nap1\taf_szido_kezd1\taf_szido_bef1"
        + "\taf_szido_vizsg1\taf_kb_tanacsa1\taf_kb_tanacsa_nev1\taf_kb_tanacsa_na1\taf_kb_tanacsa_cim1"
        + "\taf_kb_tanacsa_isz1\taf_kb_tanacsa_orsz1\taf_kb_tanacsa_telep1\taf_kb_tanacsa_szem1\taf_tip2\taf_id2"
        + "\taf_url2\taf_iktsz2\taf_korr_nr2\taf_hatido2\taf_elozm2\taf_dok_hatido2\taf_dok_fiz2\taf_dok_ar02"
        + "\taf_dok_ar12\taf_dok_penzn02\taf_dok_penzn12\taf_felt31_l2\taf_felt32_l2\taf_tov_l2\taf_kozzt2"
        + "\taf_szido_ho2\taf_szido_nap2\taf_szido_kezd2\taf_szido_bef2\taf_szido_vizsg2\taf_tip3\taf_id3\taf_url3"
        + "\taf_iktsz3\taf_korr_nr3\taf_hatido3\taf_elozm3\taf_dok_hatido3\taf_dok_fiz3\taf_dok_ar03\taf_dok_ar13"
        + "\taf_dok_penzn03\taf_dok_penzn13\taf_felt31_l3\taf_felt32_l3\taf_tov_l3\taf_kozzt3\taf_szido_ho3"
        + "\taf_szido_nap3\taf_szido_kezd3\taf_szido_bef3\taf_szido_vizsg3\taf_tip4\taf_id4\taf_url4\taf_iktsz4"
        + "\taf_korr_nr4\taf_hatido4\taf_elozm4\taf_dok_hatido4\taf_dok_fiz4\taf_dok_ar04\taf_dok_ar14\taf_dok_penzn04"
        + "\taf_dok_penzn14\taf_felt31_l4\taf_felt32_l4\taf_tov_l4\taf_kozzt4\taf_szido_ho4\taf_szido_nap4"
        + "\taf_szido_kezd4\taf_szido_bef4\taf_szido_vizsg4\taf_tip5\taf_id5\taf_url5\taf_iktsz5\taf_korr_nr5"
        + "\taf_hatido5\taf_elozm5\taf_dok_hatido5\taf_dok_fiz5\taf_dok_ar05\taf_dok_ar15\taf_dok_penzn05"
        + "\taf_dok_penzn15\taf_felt31_l5\taf_felt32_l5\taf_tov_l5\taf_kozzt5\taf_szido_ho5\taf_szido_nap5"
        + "\taf_szido_kezd5\taf_szido_bef5\taf_szido_vizsg5\taf_tip6\taf_id6\taf_url6\taf_iktsz6\taf_korr_nr6"
        + "\taf_hatido6\taf_elozm6\taf_dok_hatido6\taf_dok_fiz6\taf_dok_ar06\taf_dok_ar16\taf_dok_penzn06"
        + "\taf_dok_penzn16\taf_felt31_l6\taf_felt32_l6\taf_tov_l6\taf_kozzt6\taf_szido_ho6\taf_szido_nap6"
        + "\taf_szido_kezd6\taf_szido_bef6\taf_szido_vizsg6\taf_tip7\taf_id7\taf_url7\taf_iktsz7\taf_korr_nr7"
        + "\taf_hatido7\taf_elozm7\taf_dok_hatido7\taf_dok_fiz7\taf_dok_ar07\taf_dok_ar17\taf_dok_penzn07"
        + "\taf_dok_penzn17\taf_felt31_l7\taf_felt32_l7\taf_tov_l7\taf_kozzt7\taf_szido_ho7\taf_szido_nap7"
        + "\taf_szido_kezd7\taf_szido_bef7\taf_szido_vizsg7\tszm_db\tszm_tip1\tszm_id1\tszm_url1\tszm_iktsz1"
        + "\tszm_bsz_m11\tszm_datum1\tszm_kozzt1\tszm_tip2\tszm_id2\tszm_url2\tszm_iktsz2\tszm_bsz_m12\tszm_datum2"
        + "\tszm_kozzt2\tszm_tip3\tszm_id3\tszm_url3\tszm_iktsz3\tszm_bsz_m13\tszm_datum3\tszm_kozzt3\tszm_tip4"
        + "\tszm_id4\tszm_url4\tszm_iktsz4\tszm_bsz_m14\tszm_datum4\tszm_kozzt4\tszm_tip5\tszm_id5\tszm_url5"
        + "\tszm_iktsz5\tszm_bsz_m15\tszm_datum5\tszm_kozzt5\tszm_tip6\tszm_id6\tszm_url6\tszm_iktsz6\tszm_bsz_m16"
        + "\tszm_datum6\tszm_kozzt6\tszm_tip7\tszm_id7\tszm_url7\tszm_iktsz7\tszm_bsz_m17\tszm_datum7\tszm_kozzt7"
        + "\tszt_db\tszt_tip1\tszt_id1\tszt_url1\tszt_iktsz1\tszt_telj_ok11\tszt_rtelj_ok11\tszt_mod11\tszt_kozzt1"
        + "\tszt_sz_s01\tszt_sz_s11\tszt_sz_s1_nhuf1\tszt_sz_s_penzn01\tszt_sz_s_penzn11\tszt_sz_s_afa01"
        + "\tszt_sz_s_afasz01\tszt_idot_h1\tszt_idot_n1\tszt_idot_kzd1\tszt_idot_bef1\tszt_hatlan11\tszt_hatido01"
        + "\tszt_szkotes_dt1\tszt_tdt_ny1\tszt_tdt_ak1\tszt_tip2\tszt_id2\tszt_url2\tszt_iktsz2\tszt_telj_ok12"
        + "\tszt_rtelj_ok12\tszt_mod12\tszt_kozzt2\tszt_sz_s02\tszt_sz_s12\tszt_sz_s1_nhuf2\tszt_sz_s_penzn02"
        + "\tszt_sz_s_penzn12\tszt_sz_s_afa02\tszt_sz_s_afasz02\tszt_idot_h2\tszt_idot_n2\tszt_idot_kzd2"
        + "\tszt_idot_bef2\tszt_hatlan12\tszt_hatido02\tszt_szkotes_dt2\tszt_tdt_ny2\tszt_tdt_ak2\tszt_tip3\tszt_id3"
        + "\tszt_url3\tszt_iktsz3\tszt_telj_ok13\tszt_rtelj_ok13\tszt_mod13\tszt_kozzt3\tszt_sz_s03\tszt_sz_s13"
        + "\tszt_sz_s1_nhuf3\tszt_sz_s_penzn03\tszt_sz_s_penzn13\tszt_sz_s_afa03\tszt_sz_s_afasz03\tszt_idot_h3"
        + "\tszt_idot_n3\tszt_idot_kzd3\tszt_idot_bef3\tszt_hatlan13\tszt_hatido03\tszt_szkotes_dt3\tszt_tdt_ny3"
        + "\tszt_tdt_ak3\tszt_tip4\tszt_id4\tszt_url4\tszt_iktsz4\tszt_telj_ok14\tszt_rtelj_ok14\tszt_mod14\tszt_kozzt4"
        + "\tszt_sz_s04\tszt_sz_s14\tszt_sz_s1_nhuf4\tszt_sz_s_penzn04\tszt_sz_s_penzn14\tszt_sz_s_afa04"
        + "\tszt_sz_s_afasz04\tszt_idot_h4\tszt_idot_n4\tszt_idot_kzd4\tszt_idot_bef4\tszt_hatlan14\tszt_hatido04"
        + "\tszt_szkotes_dt4\tszt_tdt_ny4\tszt_tdt_ak4\tszt_tip5\tszt_id5\tszt_url5\tszt_iktsz5\tszt_telj_ok15"
        + "\tszt_rtelj_ok15\tszt_mod15\tszt_kozzt5\tszt_sz_s05\tszt_sz_s15\tszt_sz_s1_nhuf5\tszt_sz_s_penzn05"
        + "\tszt_sz_s_penzn15\tszt_sz_s_afa05\tszt_sz_s_afasz05\tszt_idot_h5\tszt_idot_n5\tszt_idot_kzd5\tszt_idot_bef5"
        + "\tszt_hatlan15\tszt_hatido05\tszt_szkotes_dt5\tszt_tdt_ny5\tszt_tdt_ak5\tszt_tip6\tszt_id6\tszt_url6"
        + "\tszt_iktsz6\tszt_telj_ok16\tszt_rtelj_ok16\tszt_mod16\tszt_kozzt6\tszt_sz_s06\tszt_sz_s16\tszt_sz_s1_nhuf6"
        + "\tszt_sz_s_penzn06\tszt_sz_s_penzn16\tszt_sz_s_afa06\tszt_sz_s_afasz06\tszt_idot_h6\tszt_idot_n6"
        + "\tszt_idot_kzd6\tszt_idot_bef6\tszt_hatlan16\tszt_hatido06\tszt_szkotes_dt6\tszt_tdt_ny6\tszt_tdt_ak6"
        + "\tszt_tip7\tszt_id7\tszt_url7\tszt_iktsz7\tszt_telj_ok17\tszt_rtelj_ok17\tszt_mod17\tszt_kozzt7\tszt_sz_s07"
        + "\tszt_sz_s17\tszt_sz_s1_nhuf7\tszt_sz_s_penzn07\tszt_sz_s_penzn17\tszt_sz_s_afa07\tszt_sz_s_afasz07"
        + "\tszt_idot_h7\tszt_idot_n7\tszt_idot_kzd7\tszt_idot_bef7\tszt_hatlan17\tszt_hatido07\tszt_szkotes_dt7"
        + "\tszt_tdt_ny7\tszt_tdt_ak7";
    

    @Override
    public final List<ParsedTender> parse(final RawData rawTender) {
        // prepend CSV header in front of the parsed CSV row
        final String lineWithHeader = CSV_HEADER + '\n' + rawTender.getSourceData();

        try {
            final CSVParser parser = CSVParser.parse(lineWithHeader, CSVFormat.newFormat('\t')
                .withQuote(null)
                .withFirstRecordAsHeader()
                .withRecordSeparator('\n')
                .withNullString(""));

            final List<CSVRecord> records = parser.getRecords();

            return records.stream()
                .map(HungaryOldDataTenderParser::parseRecord)
                .collect(Collectors.toList());
        } catch (IOException e) {
            logger.error("Unable to prase CSV {} because of exception", rawTender.getSourceFileName(), e);
            throw new UnrecoverableException("Unable to prase CSV.", e);
        }
    }

    /**
     * Parses tender form the given {@code record}.
     *
     * @param record
     *      CSV record to be parsed
     * @return parsed tender
     */
    private static ParsedTender parseRecord(final CSVRecord record) {
        return new ParsedTender()
            .addPublication(new ParsedPublication()
                .setIsIncluded(true)
                .setSourceId(record.get("eh_iktsz"))
                .setSource(PublicationSources.HU_KH_CSV)
                .setPublicationDate(record.get("eh_kozzt"))
                .setSourceTenderId(record.get("eh_item_id")))
            .setTitle(record.get("eh_rtart"))
            .setProcedureType(record.get("eh_eljfaj"))
            // in the column can be more NUTS codes, but we do not know how to parse it
            .setAddressOfImplementation(new ParsedAddress().addNuts(record.get("eh_nuts")))
            .addBuyer(parseBody(record, "eh_ajk_")
                .addBodyId(new BodyIdentifier()
                    .setId(record.get("ajk_pir"))
                    .setScope(BodyIdentifier.Scope.HU)
                    .setType(BodyIdentifier.Type.ORGANIZATION_ID))
                .setBuyerType(record.get("eh_ajk_tip"))
                .addMainActivity(record.get("eh_ajk_tev")))
            .addLot(new ParsedTenderLot()
                .setLotNumber(record.get("eh_er_id"))
                .addBid(new ParsedBid()
                    .setIsWinning(Boolean.TRUE.toString())
                    .addBidder(parseBody(record, "eh_ny_"))
                    .setIsConsortium(isEnabled(record.get("eh_ny_konz")).toString())
                    .setPrice(parsePrice(record, "eh_sz_s_")))
                .setBidsCount(record.get("eh_aj_szam_fv")))
            .addCpv(parseTenderMainCpv(record))
            .setBidDeadline(record.get("af_hatido1"))
            .setEstimatedPrice(parsePrice(record, "eh_b_s_"))
            .setFinalPrice(parsePrice(record, "eh_k_s_"));
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    /**
     * Checks whether the given {@code value} is equals to "1".
     * 
     * @param value
     *      value to be tested
     * @return return true only and only if the given value is equal to "1", otherwise false
     */
    private static Boolean isEnabled(final String value) {
        return value != null && value.equals("1");
    }

    /**
     * Parses price from the given {@code record}.
     *
     * @param record
     *      CSV record
     * @param prefix
     *      price columns names prefix
     * @return ParsedPrice
     */
    private static ParsedPrice parsePrice(final CSVRecord record, final String prefix) {
        ParsedPrice price = new ParsedPrice()
            .setNetAmount(record.get(prefix + "nhuf"))
            .setCurrency(record.get(prefix + "penzn0"));

        if (isEnabled(record.get(prefix + "afa_fv"))) {
            price
                .setAmountWithVat(record.get(prefix + "fv"))
                .setVat(record.get(prefix + "afasz_e_fv"));
        }

        return price;
    }

    /**
     * Parses body from the given {@code record}.
     *
     * @param record
     *      CSV record
     * @param prefix
     *      body columns names prefix
     * @return ParsedBody
     */
    private static ParsedBody parseBody(final CSVRecord record, final String prefix) {
        return new ParsedBody()
            .setName(record.get(prefix + "nev"))
            .setAddress(new ParsedAddress()
                .setStreet(record.get(prefix + "cim"))
                .setCity(record.get(prefix + "telep"))
                .setPostcode(record.get(prefix + "irsz"))
                .setUrl(record.get(prefix + "url")))
            .setPhone(record.get(prefix + "tel"))
            .setEmail(record.get(prefix + "email"))
            .addBodyId(new BodyIdentifier()
                .setId(record.get(prefix + "ksh"))
                .setScope(BodyIdentifier.Scope.HU)
                .setType(BodyIdentifier.Type.TAX_ID));
    }

    /**
     * Parses tender main CPV value from the given {@code record}.
     *
     * @param record
     *      CSV record
     * @return ParsedCPV or Null
     */
    private static ParsedCPV parseTenderMainCpv(final CSVRecord record) {
        final String mainCpvCode = record.get("eh_cpv_fsz_ft");
        if (mainCpvCode == null) {
            return null;
        }
        
        return new ParsedCPV()
            .setIsMain(Boolean.TRUE.toString())
            .setCode(mainCpvCode);
    }

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "HU";
    }
}
