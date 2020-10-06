package eu.datlab.worker.pt.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedDocument;
import eu.dl.dataaccess.dto.parsed.ParsedPayment;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Contracts parser for Portugal.
 *
 * @author Tomas Mrazek
 */
public final class BASEContractTenderParser extends BaseDatlabTenderParser {
    private static final String VERSION = "1.0";

    @Override
    public List<ParsedTender> parse(final RawData raw) {
        final Document doc = Jsoup.parse(raw.getSourceData());
        final Element contractTable = JsoupUtils.selectFirst("table:first-of-type", doc);
        final Element contractExecutionTable = JsoupUtils.selectFirst("table:last-of-type", doc);

        if (contractTable == null) {
            return Collections.emptyList();
        }

        String completionDate = BASETenderParserUtils.getFirstValueByLabel(contractExecutionTable,
            "Data de fecho do contrato");

        String procedureType = BASETenderParserUtils.getFirstValueByLabel(contractTable, "Tipo de procedimento");

        final ParsedTender tender = new ParsedTender()
            .addPublication(new ParsedPublication()
                .setIsIncluded(true)
                .setSourceFormType(PublicationFormType.CONTRACT_AWARD.name())
                .setSource(PublicationSources.PT_BASE)
                .setHumanReadableUrl(raw.getSourceUrl().toString())
                .setPublicationDate(
                    BASETenderParserUtils.getFirstValueByLabel(contractTable, "Data de publicação no BASE")))
            .addPublication(BASETenderParserUtils.parsePublicationReference(
                JsoupUtils.getFirstLabeledValueNode(contractTable, "Anúncio")))
            .setSupplyType(BASETenderParserUtils.getFirstValueByLabel(contractTable, "Tipo(\\(s\\))? de contrato"))
            .setNationalProcedureType(procedureType)
            .addBuyer(BASETenderParserUtils.parseBody(
                JsoupUtils.getFirstLabeledValueNode(contractTable, "Entidade adjudicante \\- Nome, NIF")))
            .setAcceleratedProcedureJustification(BASETenderParserUtils.getFirstValueByLabel(contractTable,
                "Fundamentação da necessidade de recurso ao ajuste direto \\(se aplicável\\)"))
            .setIsCentralProcurement(BASETenderParserUtils.parseBoolean(
                JsoupUtils.getFirstLabeledValueNode(contractTable, "Procedimento Centralizado")).toString())
            .addLot(new ParsedTenderLot()
                .setBids(parseBids(contractTable, contractExecutionTable, completionDate))
                .setCompletionDate(completionDate)
                .setDescription(BASETenderParserUtils.getFirstValueByLabel(contractTable, "Objeto do Contrato"))
                .setCpvs(BASETenderParserUtils.parseCPVs(JsoupUtils.getFirstLabeledValueNode(contractTable, "CPV")))
            )
            .setContractSignatureDate(BASETenderParserUtils.getFirstValueByLabel(contractTable,
                "Data de celebração do contrato"))
            .setEstimatedDurationInDays(BASETenderParserUtils.getFirstValueByLabel(contractTable, "Prazo de execução"))
            .setAddressOfImplementation(parseAddress(
                JsoupUtils.getFirstLabeledValueNode(contractTable, "Local de execução \\- País, Distrito, Concelho")))
            .setDocuments(parseDocuments(JsoupUtils.getFirstLabeledValueNode(contractTable, "Documentos")))
            .setModificationReason(parseModificationReason(contractExecutionTable));

        String frameworkAgreementNumber = BASETenderParserUtils.getFirstValueByLabel(contractTable,
            "Nº de registo do acordo quadro");
        if (frameworkAgreementNumber != null) {
            tender.addPublication(new ParsedPublication()
                .setSourceId(frameworkAgreementNumber)
                .setIsIncluded(false)
                .setIsParentTender(true)
                .setSource(PublicationSources.PT_BASE));
        }

        Elements priceIncreasePublicationNodes =
            JsoupUtils.select("a", JsoupUtils.getFirstLabeledValueNode(contractTable, "Incrementos superiores a 15%"));
        if (!priceIncreasePublicationNodes.isEmpty()) {
            priceIncreasePublicationNodes.stream().forEach(n -> {
                tender.addPublication(new ParsedPublication()
                    .setIsIncluded(false)
                    .setSource(PublicationSources.PT_BASE)
                    .setSourceFormType("Incrementos superiores a 15%")
                    .setPublicationDate(n.text().replace("Incremento de ", ""))
                    .setHumanReadableUrl(JsoupUtils.selectAttribute("a", "href", n)));
            });
        }

        return Collections.singletonList(tender);
    }

    /**
     * Parses modifications reason from the given {@code context}.
     *
     * @param context
     *      context that includes modification reason data
     * @return modification reason or null
     */
    private String parseModificationReason(final Element context) {
        String modificationReason = "";

        String delayReason = BASETenderParserUtils.getFirstValueByLabel(context, "Causas das alterações ao prazo");
        if (delayReason != null) {
            modificationReason = delayReason;
        }

        String priceChangeReason = BASETenderParserUtils.getFirstValueByLabel(context,
                "Causas das alterações ao preço");
        if (priceChangeReason != null) {
            modificationReason += (modificationReason.isEmpty() ? "" : "\n") + delayReason;
        }

        return modificationReason.isEmpty() ? null : modificationReason;
    }

    /**
     * Parses all bids from thew given {@code context}.
     *
     * @param context
     *      context that includes bids data
     * @param contractExecutionTable
     *      context that includes tender execution data
     * @param paymentDate
     *      date of payment
     * @return non-empty list of parsed bids or null
     */
    private List<ParsedBid> parseBids(final Element context, final Element contractExecutionTable,
        final String paymentDate) {
        if (context == null) {
            return null;
        }

        List<ParsedBid> bids = new ArrayList<>();

        List<ParsedBody> winners = BASETenderParserUtils.parseBodies(
            JsoupUtils.getFirstLabeledValueNode(context, "Entidade adjudicatária \\- Nome, NIF"));
        
        if (winners != null) {
            ParsedPrice price = BASETenderParserUtils.parsePrice(
                JsoupUtils.getFirstLabeledValueNode(context, "Preço contratual"));
            ParsedPrice paymentPrice = BASETenderParserUtils.parsePrice(
                JsoupUtils.getFirstLabeledValueNode(contractExecutionTable, "Preço total efetivo"));

            winners.stream().forEach(w -> bids.add(new ParsedBid()
                .setIsWinning(Boolean.TRUE.toString())
                .addBidder(w)
                .addPayment(paymentPrice != null ? new ParsedPayment()
                    .setPrice(paymentPrice).setPaymentDate(paymentDate) : null)
                .setPrice(price)
            ));
        }

        List<ParsedBody> losers = BASETenderParserUtils.parseBodies(
            JsoupUtils.getFirstLabeledValueNode(context, "Concorrentes"));

        if (losers != null) {
            losers.stream().forEach(l -> bids.add(new ParsedBid()
                .setIsWinning(Boolean.FALSE.toString())
                .addBidder(l)));
        }
        
        return bids.isEmpty() ? null : bids;
    }

    /**
     * Parses address from the given {@code addressNode}. Method assumes address in format <COUNTRY>, <STATE>, <CITY>.
     *
     * @param addressNode
     *      node that includes address data
     * @return parsed address or null
     */
    private ParsedAddress parseAddress(final Element addressNode) {
        if (addressNode == null) {
            return null;
        }

        String[] addressData = addressNode.text().split(", ");

        return addressData.length < 3
            ? new ParsedAddress().setRawAddress(addressNode.text())
            : new ParsedAddress()
                .setCountry(addressData[0])
                .setState(addressData[1])
                .setCity(addressData[2]);
    }

    /**
     * Parses documents.
     *
     * @param documentsNode
     *      node that includes documents data
     * @return non-empty list of parsed documents or null
     */
    private List<ParsedDocument> parseDocuments(final Element documentsNode) {
        Elements docNodes = JsoupUtils.select("a", documentsNode);
        if (docNodes == null || docNodes.isEmpty()) {
            return null;
        }

        List<ParsedDocument> documents = new ArrayList<>();
        docNodes.stream().forEach(n -> new ParsedDocument()
            .setTitle(n.text())
            .setUrl(n.attr("href")));

        return documents;
    }

    @Override
    public String getVersion() {
        return VERSION;
    }

    @Override
    protected String countryOfOrigin(final ParsedTender parsed, final RawData raw){
        return "PT";
    }

    @Override
    protected List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}
