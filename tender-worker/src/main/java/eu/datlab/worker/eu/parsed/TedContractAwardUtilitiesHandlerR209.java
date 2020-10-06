package eu.datlab.worker.eu.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

/**
 * Parser for TED contract award form with version R2.0.9.S01.E01 or newer.
 *
 * @author Tomas Mrazek
 */
public final class TedContractAwardUtilitiesHandlerR209 {
    /**
     * Suppress default constructor for noninstantiability.
     */
    private TedContractAwardUtilitiesHandlerR209() {
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
            .setIsJointProcurement(JsoupUtils.exists("JOINT_PROCUREMENT_INVOLVED", contractingBodyNode).toString())
            .setIsCentralProcurement(JsoupUtils.exists("CENTRAL_PURCHASING", contractingBodyNode).toString())
            .setTitle(JsoupUtils.selectText("TITLE", contractNode))
            .setCpvs(TedTenderParserR209Utils.parseCpvs(JsoupUtils.selectFirst("CPV_MAIN", contractNode)))
            .setSupplyType(JsoupUtils.selectAttribute("TYPE_CONTRACT", "CTYPE", contractNode))
            .setDescription(JsoupUtils.selectText("SHORT_DESCR", contractNode))
            .setHasLots(JsoupUtils.exists("LOT_DIVISION", contractNode).toString())
            .setFinalPrice(TedTenderParserR209Utils.parsePriceWithRange(contractNode))
            .setLots(TedTenderParserR209Utils.parseLots(JsoupUtils.select("OBJECT_DESCR", contractNode), originNode))
            .setNationalProcedureType(procedureType)
            .setNpwpReasons(TedTenderParserR209Utils.parseNpwpReasons(
                JsoupUtils.selectFirst("PT_AWARD_CONTRACT_WITHOUT_CALL D_JUSTIFICATION", procedureNode)))
            .setIsFrameworkAgreement(JsoupUtils.exists("FRAMEWORK", procedureNode).toString())
            .setIsElectronicAuction(JsoupUtils.exists("EAUCTION_USED", procedureNode).toString())
            .setIsCoveredByGpa(JsoupUtils.exists("CONTRACT_COVERED_GPA", procedureNode).toString())
            .setIsDps(JsoupUtils.exists("DPS", procedureNode).toString())
            .setAppealBodyName(JsoupUtils.selectText("ADDRESS_REVIEW_BODY", complementaryInfoNode))
            .setMediationBodyName(JsoupUtils.selectText("ADDRESS_MEDIATION_BODY", complementaryInfoNode));

        return parsedTender;
    }
}
