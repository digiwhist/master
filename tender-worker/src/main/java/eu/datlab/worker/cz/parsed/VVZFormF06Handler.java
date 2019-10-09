package eu.datlab.worker.cz.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

/**
 * Handler for parsing form F06 - Contract award notice – utilities.
 */
final class VVZFormF06Handler extends VVZContractAwardHandler {

    /**
     * Suppress default constructor for noninstantiability.
     */
    private VVZFormF06Handler() {
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
        ParsedTender parsedTender = tender;

        final Element sectionI = VVZTenderParser.getSectionI(form);
        final Element sectionII = VVZTenderParser.getSectionII(form);
        final Element sectionIV = VVZTenderParser.getSectionIV(form);
        final Element sectionVI = VVZTenderParser.getSectionVI(form);
        final Element annexD = getAnnexD(form);

        // SECTION I

        // subsection I.1
        parsedTender.setBuyers(parseBuyers(sectionI));

        // subsection I.2
        parsedTender.setIsCentralProcurement(parseCentralProcurement(sectionI))
                .setIsJointProcurement(parseJointProcurement(sectionI));

        // subsection I.6
        parsedTender.getBuyers().get(0).addMainActivity(parseBuyerMainActivityFromI6(sectionI));

        // SECTION II

        // subsection II.1.1
        parsedTender.setTitle(VVZTenderParser.parseTenderTitle(sectionII));

        // subsection II.1.2
        parsedTender.setCpvs(parseTenderCPVCodes(sectionII));

        // subsection II.1.3
        parsedTender.setSupplyType(VVZTenderParser.parseSupplyType(sectionII));

        // subsection II.1.4
        parsedTender.setDescription(parseTenderDescription(sectionII));

        // subsection II.1.6
        parsedTender.setHasLots(VVZTenderParser.parseHasLots(sectionII));

        // subsection II.1.7
        parsedTender.setFinalPrice(parseTenderFinalPrice(sectionII));

        // subsection II.2
        parsedTender.setLots(parseLotsFromSubsectionII2(form));

        // SECTION IV

        // subsection IV.1.1
        parsedTender.setNationalProcedureType(VVZTenderParser.parseProcedureType(sectionIV));

        // subsection IV.1.3
        parsedTender.setIsFrameworkAgreement(parseIsFrameworkAgreement(sectionIV)).setIsDps(parseIsDPS(sectionIV));

        // subsection IV.1.6
        parsedTender.setIsElectronicAuction(VVZTenderParser.parseIsElectronicAuction(sectionIV));

        // subsection IV.1.8
        parsedTender.setIsCoveredByGpa(parseIsCoveredByGPA(sectionIV));

        // subsection IV.2.1
        parsedTender.addPublication(parsePreviousTedPublication(sectionIV));

        // subsection IV.2.8
        parsedTender = parseDPSCancellationInfo(sectionIV, parsedTender, form);

        // SECTION V
        parsedTender.addLots(parseLotsAwards(form, Boolean.TRUE.toString().equals(parsedTender.getIsFrameworkAgreement())));

        // SECTION VI

        // subsection VI.3)
        parsedTender.setAdditionalInfo(VVZTenderParser.parseAdditionalInfo(sectionVI));

        // subsection VI.4.1
        parsedTender.setAppealBodyName(parseAppealBodyName(sectionVI));

        // subsection VI.4.2
        parsedTender.setMediationBodyName(parseMediationBodyName(sectionVI));

        // attachment D1/D2
        parsedTender.setNpwpReasons(parseNpwpReasons(annexD));

        return parsedTender;
    }
}
