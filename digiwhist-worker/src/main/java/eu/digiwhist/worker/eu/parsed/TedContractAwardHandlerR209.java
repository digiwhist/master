package eu.digiwhist.worker.eu.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

/**
 * Parser for TED contract award form with version R2.0.9.S01.E01 or newer.
 *
 * @author Tomas Mrazek
 */
public final class TedContractAwardHandlerR209 {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedContractAwardHandlerR209() {
        throw new AssertionError();
    }

    /**
     * Parses contract notice form specific data.
     *
     * @param parsedTender
     *         parsed tender
     * @param document
     *         parsed document
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        final Element originNode = TedTenderParserUtils.getOriginNode(document);

        final Element contractingBodyNode = TedTenderParserR209Utils.getSectionI(originNode);
        final Element contractNode = TedTenderParserR209Utils.getSectionII(originNode);
        final Element procedureNode = TedTenderParserR209Utils.getSectionIV(originNode);
        final Element complementaryInfoNode = TedTenderParserR209Utils.getSectionVI(originNode);

        String procedureType = TedTenderParserR209Utils.parseProcedureType(procedureNode);

        parsedTender.setBuyers(TedTenderParserR209Utils.parseBuyers(document))
            .setTitle(JsoupUtils.selectText("TITLE", contractNode))
            .setProcedureType(procedureType)
            .setNationalProcedureType(procedureType)
            .setSupplyType(JsoupUtils.selectAttribute("TYPE_CONTRACT", "CTYPE", contractNode))
            .setDescription(JsoupUtils.selectText("SHORT_DESCR", contractNode))
            .setIsJointProcurement(JsoupUtils.exists("JOINT_PROCUREMENT_INVOLVED", contractingBodyNode).toString())
            .setIsCentralProcurement(JsoupUtils.exists("CENTRAL_PURCHASING", contractingBodyNode).toString())
            .setHasLots(JsoupUtils.exists("LOT_DIVISION", contractNode).toString())
            .setLots(TedTenderParserR209Utils.parseLots(JsoupUtils.select("OBJECT_DESCR", contractNode), originNode))
            .setIsFrameworkAgreement(JsoupUtils.exists("FRAMEWORK", procedureNode).toString())
            .setIsDps(JsoupUtils.exists("DPS", procedureNode).toString())
            .setIsElectronicAuction(JsoupUtils.exists("EAUCTION_USED", procedureNode).toString())
            .setIsCoveredByGpa(JsoupUtils.exists("CONTRACT_COVERED_GPA", procedureNode).toString())
            .setAppealBodyName(JsoupUtils.selectText("ADDRESS_REVIEW_BODY", complementaryInfoNode))
            .setMediationBodyName(JsoupUtils.selectText("ADDRESS_MEDIATION_BODY", complementaryInfoNode))
            .setCpvs(TedTenderParserR209Utils.parseCpvs(JsoupUtils.selectFirst("CPV_MAIN", contractNode)))
            .setFinalPrice(TedTenderParserR209Utils.parsePriceWithRange(contractNode))
            .setNpwpReasons(TedTenderParserR209Utils.parseNpwpReasons(
                JsoupUtils.selectFirst("PT_AWARD_CONTRACT_WITHOUT_CALL D_JUSTIFICATION", procedureNode)));

        return parsedTender;
    }
}
