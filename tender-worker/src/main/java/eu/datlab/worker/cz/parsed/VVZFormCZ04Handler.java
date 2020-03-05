package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedCorrigendum;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.apache.commons.lang3.StringUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for parsing form CZ04 - Oprava národního formuláře (Notice for changes or additional information for
 * under-the-threshold tenders).
 */
final class VVZFormCZ04Handler extends VVZCzFormsHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZFormCZ04Handler.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormCZ04Handler() {
        throw new AssertionError();
    }

    /**
     * Parses Form specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from Form
     */
    public static ParsedTender parseFormAttributes(final ParsedTender tender, final Document form) {

        final Element sectionI = VVZTenderParser.getSectionI(form);
        final Element sectionII = VVZTenderParser.getSectionII(form);
        final Element sectionVI = getSectionVI(form);

        // parse info about publication of related original form that is being corrected
        tender.addPublication(VVZTenderParser.parseRelatedOriginalPublicationFromHeader(form));

        // SECTION I

        // subsection I.1)
        tender.addBuyer(parseBuyer(sectionI));

        // SECTION II

        // subsection II.1.1)
        tender.setTitle(VVZTenderParser.parseTenderTitle(sectionII));

        // subsection II.1.2)
        tender.setSupplyType(VVZTenderParser.parseSupplyType(sectionII))
                .addCpv(parseCpvCode(sectionII))
                .setAddressOfImplementation(parseAddressOfImplementation(sectionII));

        // subsection II.1.3)
        tender.setDescription(parseTenderDescription(sectionII));

        // SECTION V

        // subsection V.3)
        tender.addPublication(parseRelatedOriginalPublication(form));

        // SECTION VI

        // subsection VI.1)
        tender.setCorrections(parseCorrections(sectionVI));

        // subsection VI.2)
        tender.setAdditionalInfo(parseAdditionalInfo(sectionVI));

        return tender;
    }

    /**
     * Gets html for section VI.
     *
     * @param form
     *         form html
     *
     * @return section VI html
     */
    private static Element getSectionVI(final Document form) {
        return form.select("div#Change").first();
    }

    // ---------------------------------
    // CZ SUBSECTION V.3)
    // ---------------------------------

    /**
     * Parses original form publication.
     *
     * @param sectionV
     *         section V html
     *
     * @return publication of original form
     */
    static ParsedPublication parseRelatedOriginalPublication(final Element sectionV) {
        parseRelatedOriginalPublicationFormId(sectionV);
        return new ParsedPublication().setIsIncluded(false)
                .setSource(VVZTenderParser.VESTNIK_SOURCE_URL)
                .setSourceId(parseRelatedOriginalPublicationFormId(sectionV))
                .setSourceTenderId(parseRelatedOriginalPublicationTenderId(sectionV))
                .setDispatchDate(parseRelatedOriginalPublicationSentDate(sectionV));
    }

    /**
     * Parses original form tender id.
     *
     * @param sectionV
     *         section V html
     *
     * @return original form tender id
     */
    private static String parseRelatedOriginalPublicationTenderId(final Element sectionV) {
        return VVZTenderParserUtils.getFieldValue(sectionV, ".*\\.ContractNumber$");
    }

    /**
     * Parses original form notice number.
     *
     * @param sectionV
     *         section V
     *
     * @return original form notice number (form id)
     */
    private static String parseRelatedOriginalPublicationFormId(final Element sectionV) {
        return VVZTenderParserUtils.getFieldValue(sectionV, ".*\\.NoticeNumber$");
    }

    /**
     * Parses original form sent date.
     *
     * @param sectionV
     *         section V html
     *
     * @return original form sent date
     */
    private static String parseRelatedOriginalPublicationSentDate(final Element sectionV) {
        return VVZTenderParserUtils.getFieldValue(sectionV, ".*\\.DateDispatchOriginal$");
    }

    // =================================
    // CZ SECTION VI
    // =================================

    // ---------------------------------
    // CZ SUBSECTION VI.1)
    // ---------------------------------

    /**
     * Parses all the corrections.
     *
     * @param sectionVI
     *         section VI html
     *
     * @return list of corrections
     */
    static List<ParsedCorrigendum> parseCorrections(final Element sectionVI) {
        final List<ParsedCorrigendum> corrections = new ArrayList<>();

        corrections.addAll(VVZTenderParser.parseTextCorrections(sectionVI));
        corrections.addAll(parseCpvCorrections(sectionVI));
        corrections.addAll(VVZTenderParser.parseDateCorrections(sectionVI));
        corrections.addAll(parseValueCorrections(sectionVI));

        return corrections;
    }

    /**
     * Parses CPV corrections.
     *
     * @param sectionVI
     *         section VI html
     *
     * @return CPV corrections
     */
    private static List<ParsedCorrigendum> parseCpvCorrections(final Element sectionVI) {
        final List<ParsedCorrigendum> cpvCorrections = new ArrayList<>();

        final Elements cpvChangesHtmls = VVZTenderParser.getCpvChangesHtmls(sectionVI);
        for (Element cpvChangeHtml : cpvChangesHtmls) {
            final String sectionNumber = VVZTenderParser.parseCorrectionSectionNumber(cpvChangeHtml);
            if (StringUtils.isNotEmpty(sectionNumber)) {
                final ParsedCorrigendum corrigendum = new ParsedCorrigendum().setSectionNumber(sectionNumber)
                        .setLotNumber(VVZTenderParser.parseCorrectionLotNumber(cpvChangeHtml))
                        .setPlaceOfModifiedText(VVZTenderParser.parseCorrectionPlace(cpvChangeHtml))
                        .addOriginalCpv(parseCorrectionOriginalCpv(cpvChangeHtml))
                        .addReplacementCpv(parseCorrectionReplacementCpv(cpvChangeHtml));
                cpvCorrections.add(corrigendum);
            }
        }
        return cpvCorrections;
    }

    /**
     * Parses price corrections.
     *
     * @param sectionVI
     *         section VI html
     *
     * @return price corrections
     */
    private static List<ParsedCorrigendum> parseValueCorrections(final Element sectionVI) {
        final List<ParsedCorrigendum> valueCorrections = new ArrayList<>();

        final Elements valueChangesHtmls = getValueChangesHtmls(sectionVI);
        for (Element valueChangeHtml : valueChangesHtmls) {
            final String sectionNumber = VVZTenderParser.parseCorrectionSectionNumber(valueChangeHtml);
            if (StringUtils.isNotEmpty(sectionNumber)) {
                final ParsedCorrigendum corrigendum = new ParsedCorrigendum().setSectionNumber(sectionNumber)
                        .setLotNumber(VVZTenderParser.parseCorrectionLotNumber(valueChangeHtml))
                        .setPlaceOfModifiedText(VVZTenderParser.parseCorrectionPlace(valueChangeHtml))
                        .setOriginalValue(parseCorrectionOriginalValue(valueChangeHtml))
                        .setReplacementValue(parseCorrectionReplacementValue(valueChangeHtml));
                valueCorrections.add(corrigendum);
            }
        }
        return valueCorrections;
    }

    /**
     * Gets price corrections html fragments.
     *
     * @param sectionVI
     *         section VI html
     *
     * @return price corrections html fragments
     */
    private static Elements getValueChangesHtmls(final Element sectionVI) {
        return sectionVI.select("div[model~=.*\\.ChangeValueList\\[\\d+\\]$]");
    }

    /**
     * Parses correction original CPV.
     *
     * @param changeHtml
     *         change html
     *
     * @return correction orginal CPV
     */
    private static ParsedCPV parseCorrectionOriginalCpv(final Element changeHtml) {
        return new ParsedCPV().setIsMain(Boolean.TRUE.toString())
                .setCode(VVZTenderParserUtils.getFieldValue(changeHtml, ".*\\.OldCpv\\.CpvMain$"));
    }

    /**
     * Parses correction replacement CPV.
     *
     * @param changeHtml
     *         change html
     *
     * @return correction replacement CPV
     */
    private static ParsedCPV parseCorrectionReplacementCpv(final Element changeHtml) {
        return new ParsedCPV().setIsMain(Boolean.TRUE.toString())
                .setCode(VVZTenderParserUtils.getFieldValue(changeHtml, ".*\\.NewCpv\\.CpvMain$"));
    }

    /**
     * Parses correction original price.
     *
     * @param changeHtml
     *         change html
     *
     * @return correction original price
     */
    private static ParsedPrice parseCorrectionOriginalValue(final Element changeHtml) {
        return new ParsedPrice().setCurrency("CZK")
                .setNetAmount(VVZTenderParserUtils.getFieldValue(changeHtml, ".*\\.OldValue\\.ValueFrom$"));
    }

    /**
     * Parses correction replacement price.
     *
     * @param changeHtml
     *         change html
     *
     * @return correction replacement price
     */
    private static ParsedPrice parseCorrectionReplacementValue(final Element changeHtml) {
        return new ParsedPrice().setCurrency("CZK")
                .setNetAmount(VVZTenderParserUtils.getFieldValue(changeHtml, ".*\\.NewValue\\.ValueFrom$"));
    }

    // ---------------------------------
    // CZ SUBSECTION VI.2)
    // ---------------------------------

    /**
     * Parses additional tender info.
     *
     * @param root
     *         html segment for parsing
     *
     * @return additional tender info
     */
    static String parseAdditionalInfo(final Element root) {
        return VVZTenderParserUtils.getFieldValue(root, ".*\\.InfoAdd$");
    }
}
