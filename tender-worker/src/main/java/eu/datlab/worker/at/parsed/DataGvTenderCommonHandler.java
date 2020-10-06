package eu.datlab.worker.at.parsed;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import org.jsoup.nodes.Element;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Common handler for all form types.
 *
 * @author Miroslav Brezik
 */
abstract class DataGvTenderCommonHandler {

    /**
     * @param base XML element containing name_id_phone_email_type type
     * @return ParsedBody instance
     */
    public static ParsedBody getNameIdPhoneEmailType(final Element base) {
        BodyIdentifier id = new BodyIdentifier()
                .setId(base.selectFirst("NATIONALID").text())
                .setType(BodyIdentifier.Type.ORGANIZATION_ID);

        ParsedBody body = new ParsedBody()
                .setName(base.selectFirst("OFFICIALNAME").text())
                .setBodyIds(Arrays.asList(id));

        Optional.ofNullable(base.selectFirst("PHONE")).map(Element::text).ifPresent(body::setPhone);
        Optional.ofNullable(base.selectFirst("E_MAIL")).map(Element::text).ifPresent(body::setEmail);

        return body;
    }

    /**
     * Parses title and updates passed passedTender.
     * @param parsedTender tender to be updated
     * @param form XML form root
     * @return updated parsedTender
     */
    public static ParsedTender parseTitle(final ParsedTender parsedTender, final Element form) {
        return parsedTender.setTitle(form.selectFirst("OBJECT_CONTRACT > TITLE > P").text());
    }

    /**
     * @param base XML element containing cpv_set type
     * @return ParsedCPV instance
     */
    public static ParsedCPV getCpv(final Element base) {
        return new ParsedCPV()
                .setCode(base.selectFirst("CPV_CODE").attr("CODE"))
                .setIsMain(String.valueOf(base.tagName().equals("CPV_MAIN")));
    }

    /**
     * Parses supply type and updates passed passedTender.
     * @param parsedTender tender to be updated
     * @param form XML form root
     * @return updated parsedTender
     */
    public static ParsedTender parseTypeContract(final ParsedTender parsedTender, final Element form) {
        return parsedTender
                .setSupplyType(form.selectFirst("OBJECT_CONTRACT > TYPE_CONTRACT").attr("CTYPE"));
    }

    /**
     * @param base XML element containing text_ft_multi_lines type
     * @return concatenated strings
     */
    public static String getTextFtMultiLines(final Element base) {
        return base.select("P").stream()
                .map(Element::text)
                .collect(Collectors.joining(" "));
    }

    /**
     * Parses lots and updates passed passedTender.
     * @param parsedTender tender to be updated
     * @param form XML form root
     * @return updated parsedTender
     */
    public static ParsedTender parseLots(final ParsedTender parsedTender, final Element form) {
        ParsedTenderLot lot = new ParsedTenderLot();

        Element objectDescr = form.selectFirst("OBJECT_CONTRACT > OBJECT_DESCR");
        boolean anyfieldSet = false;
        String estimatedDurationInYears = Optional.ofNullable(objectDescr.selectFirst("DURATION[TYPE='YEAR']"))
                .map(Element::text)
                .orElse(null);
        if (estimatedDurationInYears != null) {
            lot.setEstimatedDurationInYears(estimatedDurationInYears);
            anyfieldSet = true;
        }
        String estimatedDurationInMonths = Optional.ofNullable(objectDescr.selectFirst("DURATION[TYPE='MONTH']"))
                .map(Element::text)
                .orElse(null);
        if (estimatedDurationInMonths != null) {
            lot.setEstimatedDurationInMonths(estimatedDurationInMonths);
            anyfieldSet = true;
        }
        String estimatedDurationInDays = Optional.ofNullable(objectDescr.selectFirst("DURATION[TYPE='DAY']"))
                .map(Element::text)
                .orElse(null);
        if (estimatedDurationInDays != null) {
            lot.setEstimatedDurationInDays(estimatedDurationInDays);
            anyfieldSet = true;
        }
        String estimatedStartDate = Optional.ofNullable(objectDescr.selectFirst("DATE_START"))
                .map(Element::text)
                .orElse(null);
        if (estimatedStartDate != null) {
            lot.setEstimatedStartDate(estimatedStartDate);
            anyfieldSet = true;
        }
        String estimatedCompletionDate = Optional.ofNullable(objectDescr.selectFirst("DATE_END"))
                .map(Element::text)
                .orElse(null);
        if (estimatedCompletionDate != null) {
            lot.setEstimatedCompletionDate(estimatedCompletionDate);
            anyfieldSet = true;
        }

        if (anyfieldSet) {
            parsedTender.setLots(Arrays.asList(lot));
        }

        return parsedTender;
    }

    /**
     * @param base XML element containing contractor_type type
     * @return ParsedBody instance
     */
    public static ParsedBody getContractorType(final Element base) {
        Element addressContractor = base.selectFirst("ADDRESS_CONTRACTOR");
        ParsedBody parsedBody = new ParsedBody().setName(addressContractor.selectFirst("OFFICIALNAME").text());
        Element nationalid = addressContractor.selectFirst("NATIONALID");
        if (nationalid != null) {
            parsedBody.setBodyIds(Arrays.asList(
                    new BodyIdentifier()
                            .setId(nationalid.text())
                            .setType(BodyIdentifier.Type.ORGANIZATION_ID)));
        }

        return parsedBody;
    }

    /**
     * @param base XML element containing val type
     * @return ParsedPrice instance
     */
    public static ParsedPrice getVal(final Element base) {
        return new ParsedPrice()
                .setNetAmount(base.text())
                .setCurrency(base.attr("CURRENCY"));
    }

    /**
     * Parses procedure and updates passed passedTender.
     * @param parsedTender tender to be updated
     * @param form XML form root
     * @return updated parsedTender
     */
    public static ParsedTender parseProcedure(final ParsedTender parsedTender, final Element form) {
        Element procedure = form.selectFirst("PROCEDURE");
        parsedTender.setNationalProcedureType(
                procedure.select("*")
                        .stream()
                        .map(Element::tagName)
                        .filter(t -> t.startsWith("PT_"))
                        .collect(Collectors.joining(",")));

        Element framework = procedure.selectFirst("FRAMEWORK");
        if (framework != null) {
            parsedTender.setIsFrameworkAgreement("true");
        }
        Element dps = procedure.selectFirst("dps");
        if (dps != null) {
            parsedTender.setIsDps("true");
        }
        Optional.ofNullable(procedure.selectFirst("DATETIME_RECEIPT_TENDERS")).map(Element::text)
                .ifPresent(parsedTender::setBidDeadline);

        return parsedTender;
    }

    /**
     * Parses document info and updates passed passedTender.
     * @param parsedTender tender to be updated
     * @param form XML form root
     * @return updated parsedTender
     */
    public static ParsedTender parseGroupDocumentUrlMan(final ParsedTender parsedTender, final Element form) {
        Element documentAccess = form.selectFirst("CONTRACTING_BODY > DOCUMENT_FULL, CONTRACTING_BODY > DOCUMENT_RESTRICTED");
        if (documentAccess == null) {
            return parsedTender;
        }

        if (documentAccess.tagName().equals("DOCUMENT_FULL")) {
            parsedTender.setIsDocumentsAccessRestricted("false");
        } else if (documentAccess.tagName().equals("DOCUMENT_RESTRICTED")) {
            parsedTender.setIsDocumentsAccessRestricted("true");
        }
        parsedTender.setDocumentsLocation(
                new ParsedAddress().setUrl(form.selectFirst("CONTRACTING_BODY > URL_DOCUMENT").text()));

        return parsedTender;
    }

    /**
     * @param base XML element containing nuts type
     * @return list of nuts codes
     */
    public static List<String> getNuts(final Element base) {
        return base.select("NUTS").stream()
                .map(e -> e.attr("CODE"))
                .collect(Collectors.toList());
    }

    /**
     * Parses lots info and updates passed passedTender.
     * @param parsedTender tender to be updated
     * @param form XML form root
     * @return updated parsedTender
     */
    public static ParsedTender parseLotDivision(final ParsedTender parsedTender, final Element form) {
        Element lotDivision = form.selectFirst("OBJECT_CONTRACT > LOT_DIVISION, OBJECT_CONTRACT > NO_LOT_DIVISION");
        if (lotDivision.tagName().equals("LOT_DIVISION")) {
            parsedTender.setHasLots("true");
        } else if (lotDivision.tagName().equals("NO_LOT_DIVISION")) {
            parsedTender.setHasLots("false");
        }

        return parsedTender;
    }


}

















