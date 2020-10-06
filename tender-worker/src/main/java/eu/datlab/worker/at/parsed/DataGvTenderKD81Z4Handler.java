package eu.datlab.worker.at.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import org.jsoup.nodes.Element;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Handler for form type KD_8_1_Z4.
 *
 * @author Miroslav Brezik
 */
final class DataGvTenderKD81Z4Handler extends DataGvTenderCommonHandler {

    /**
     * Default constructor suppression.
     */
    private DataGvTenderKD81Z4Handler() {
    }

    /**
     * Parses form specific fields and updates the passed tender.
     *
     * @param parsedTender
     *         tender to be updated with parsed data
     * @param form
     *         parsed XML document from downloaded data
     *
     * @return updated tender object
     */
    public static ParsedTender parseTender(final ParsedTender parsedTender, final Element form) {
        // CONTRACTING_BODY
        parsedTender.setBuyers(Arrays.asList(
                getNameIdPhoneEmailType(form.selectFirst("CONTRACTING_BODY > ADDRESS_CONTRACTING_BODY"))));

        // OBJECT_CONTRACT
        parseTitle(parsedTender, form);
        List<ParsedCPV> cpvs = new LinkedList<>();
        cpvs.add(getCpv(form.selectFirst("OBJECT_CONTRACT > CPV_MAIN")));
        cpvs.addAll(
                form.select("OBJECT_CONTRACT > OBJECT_DESCR > CPV_ADDITIONAL").stream()
                        .map(DataGvTenderCommonHandler::getCpv)
                        .collect(Collectors.toList()));
        parsedTender.setCpvs(cpvs);
        parseTypeContract(parsedTender, form);
        parsedTender.setDescription(getTextFtMultiLines(form.selectFirst("OBJECT_CONTRACT > SHORT_DESCR")));
        ParsedAddress parsedAddress = new ParsedAddress()
                .setNuts(getNuts(form.selectFirst("OBJECT_CONTRACT > OBJECT_DESCR")));
        Optional.ofNullable(form.selectFirst("OBJECT_CONTRACT > OBJECT_DESCR > MAIN_SITE"))
                .map(DataGvTenderCommonHandler::getTextFtMultiLines)
                .ifPresent(parsedAddress::setRawAddress);
        parsedTender.setAddressOfImplementation(parsedAddress);
        parseLots(parsedTender, form);
        parseLotDivision(parsedTender, form);

        // AWARD_CONTRACT

        // COMPLEMENTARY_INFO

        // ADDITIONAL_CORE_DATA

        return parsedTender;
    }


}
