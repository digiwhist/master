package eu.digiwhist.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedCorrigendum;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.apache.commons.lang.StringUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * Handler for parsing form F14 - Notice for changes or additional information.
 */
final class VVZFormF14Handler extends VVZEuFormsHandler {
    private static final Logger logger = LoggerFactory.getLogger(VVZFormF14Handler.class);

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormF14Handler() {
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
        final Element sectionVI = VVZTenderParser.getSectionVI(form);
        final Element sectionVII = getSectionVII(form);

        // parse info about publication of related original form that is being corrected
        tender.addPublication(VVZTenderParser.parseRelatedOriginalPublicationFromHeader(form));

        // SECTION I

        // subsection I.1
        tender.setBuyers(parseBuyers(sectionI));

        // SECTION II

        // subsection II.1.1
        tender.setTitle(VVZTenderParser.parseTenderTitle(sectionII));

        // subsection II.1.2
        tender.setCpvs(parseTenderCPVCodes(sectionII));

        // subsection II.1.3
        tender.setSupplyType(VVZTenderParser.parseSupplyType(sectionII));

        // subsection II.1.4
        tender.setDescription(parseTenderDescription(sectionII));

        // SECTION VI

        // subsection VI.6
        tender.addPublication(parseOriginalTedPublication(sectionVI));

        // SECTION VII

        // subsection VII.1
        tender.setCorrections(parseCorrections(sectionVII));

        return tender;
    }

    /**
     * Gets section VII html segment.
     *
     * @param form
     *         form html
     *
     * @return section VII html segment
     */
    private static Element getSectionVII(final Document form) {
        return form.select("div#Change").first();
    }

    // ---------------------------------
    // SUBSECTION VI.6)
    // ---------------------------------

    /**
     * Parses original publication in TED.
     *
     * @param sectionVI
     *         section VI html
     *
     * @return original publication from TED
     */
    static ParsedPublication parseOriginalTedPublication(final Element sectionVI) {
        final String formId = VVZTenderParserUtils.getFieldValue(sectionVI, ".*\\.NoticeNumber$");
        if (StringUtils.isNotEmpty(formId)) {
            return new ParsedPublication().setIsIncluded(false).setSource(VVZTenderParser.TED_SOURCE_URL);
        }
        return null;
    }

    // =================================
    // SECTION VII
    // =================================

    // ---------------------------------
    // SUBSECTION VII.1)
    // ---------------------------------

    /**
     * Parses corrections.
     *
     * @param sectionVII
     *         section VII html
     *
     * @return parsed corrections
     */
    static List<ParsedCorrigendum> parseCorrections(final Element sectionVII) {
        final List<ParsedCorrigendum> corrections = new ArrayList<>();

        corrections.addAll(VVZTenderParser.parseTextCorrections(sectionVII));
        corrections.addAll(parseCpvCorrections(sectionVII));
        corrections.addAll(VVZTenderParser.parseDateCorrections(sectionVII));

        return corrections;
    }

    /**
     * Parses CPV corrections.
     *
     * @param sectionVII
     *         section VII html
     *
     * @return CPV corrections
     */
    private static List<ParsedCorrigendum> parseCpvCorrections(final Element sectionVII) {
        final List<ParsedCorrigendum> cpvCorrections = new ArrayList<>();

        final Elements cpvChangesHtmls = VVZTenderParser.getCpvChangesHtmls(sectionVII);
        for (Element cpvChangeHtml : cpvChangesHtmls) {
            final String sectionNumber = VVZTenderParser.parseCorrectionSectionNumber(cpvChangeHtml);
            if (StringUtils.isNotEmpty(sectionNumber)) {
                final ParsedCorrigendum corrigendum = new ParsedCorrigendum().setSectionNumber(sectionNumber)
                        .setLotNumber(VVZTenderParser.parseCorrectionLotNumber(cpvChangeHtml))
                        .setPlaceOfModifiedText(VVZTenderParser.parseCorrectionPlace(cpvChangeHtml))
                        .setOriginalCpvs(parseCorrectionOriginalCpvs(cpvChangeHtml))
                        .setReplacementCpvs(parseCorrectionReplacementCpvs(cpvChangeHtml));
                cpvCorrections.add(corrigendum);
            }
        }
        return cpvCorrections;
    }

    /**
     * Parses correction original CPV.
     *
     * @param changeHtml
     *         change html
     *
     * @return correction original CPV
     */
    private static List<ParsedCPV> parseCorrectionOriginalCpvs(final Element changeHtml) {
        final Element originalCpvsDiv = changeHtml.select("div:contains(Čtěte)").last();
        return VVZTenderParser.parseCPVCodes(originalCpvsDiv);
    }

    /**
     * Parses correction replacement CPV.
     *
     * @param changeHtml
     *         change html
     *
     * @return correction replacement CPV
     */
    private static List<ParsedCPV> parseCorrectionReplacementCpvs(final Element changeHtml) {
        final Element replacementCpvsDiv = changeHtml.select("div:contains(Namísto)").last();
        return VVZTenderParser.parseCPVCodes(replacementCpvsDiv);
    }
}
