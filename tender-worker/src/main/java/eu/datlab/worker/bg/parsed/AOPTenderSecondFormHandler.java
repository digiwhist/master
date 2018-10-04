package eu.datlab.worker.bg.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.List;

import static eu.datlab.dataaccess.dto.codetables.PublicationSources.BG_AOP;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectFirst;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectText;

/**
 * Created by michalriha on 12/07/2017.
 */
public final class AOPTenderSecondFormHandler {
    /**
     * Supress default constructor for noninstatiability.
     */
    private AOPTenderSecondFormHandler() {
    }

    /**
     * Parses First form type data.
     *
     * @param doc    parsed document
     * @param sourceFormType    source form type
     * @param url    url
     *
     * @return parsed tender
     */
    public static ParsedTender parse(final Document doc, final String sourceFormType, final String url) {
        return new ParsedTender()
                .setBuyerAssignedId(selectText("body > font", doc))
                .addPublication(new ParsedPublication()
                        .setSourceFormType(sourceFormType)
                        .setIsIncluded(true)
                        .setSource(BG_AOP)
                        .setHumanReadableUrl(url)
                        .setPublicationDate(getDivUnderSpan("Дата на изпращане на настоящото обявление", doc))
                )
                .addBuyer(parseBuyer(doc))
                .setAdditionalInfo(getTextInP("Адреси и места за контакти, от които може да се получи допълнителна " +
                        "информация", doc))
                .setIsOnBehalfOf(getDivUnderSpan("Възлагащият орган извършва покупка от името на други въ" +
                        "злагащи органи", doc))
                .setTitle(getDivUnderSpan("Заглавие на поръчката, предоставено от възлагащия орган", doc))
                .setDescription(getDivUnderSpan("Кратко описание на поръчката или покупката/покупките", doc))
                .addCpv(new ParsedCPV()
                        .setIsMain(Boolean.TRUE.toString())
                        .setCode(getDivUnderSpan("CPV", doc))
                )
                .setIsCoveredByGpa(getDivUnderSpan("GPA", doc))
                .setHasLots(getDivUnderSpan("Разделяне на обособени позиции", doc))
                .setAreVariantsAccepted(getDivUnderSpan("Ще бъдат приемани варианти", doc))
                .setEstimatedPrice(new ParsedPrice()
                        .setNetAmount(getDivUnderSpan("Прогнозна стойност без ДДС", doc))
                        .setCurrency(getDivUnderSpan("Прогнозна стойност без ДДС", doc))
                )
                .setHasOptions(getDivUnderSpan("Опции", doc))
                .setEstimatedDurationInMonths(getDivUnderSpan("Продължителност в месеци", doc))
                .setDeposits(getDivUnderSpan("Изискуеми депозити и гаранции", doc))
                .setPersonalRequirements(getDivUnderSpan("Лично състояние към икономическите оператори", doc))
                .setEconomicRequirements(getDivUnderSpan("Икономически и финансови възможности", doc))
                .setTechnicalRequirements(getDivUnderSpan("Технически възможности", doc))
                .setProcedureType(getDivUnderSpan("Вид процедура", doc))
                .setSelectionMethod(getDivUnderSpan("Критерии за възлагане", doc))
                .setAwardCriteria(parseAwardCriteria(doc))
                .setIsElectronicAuction(getDivUnderSpan("Ще се използва електронен търг", doc))
                .setDocumentsDeadline(getDivUnderSpan("Срок за получаване на искания за документи или за до" +
                        "стъп до документи", doc))
                .setDocumentsPayable(getDivUnderSpan("Платими документи", doc))
                .setEnquiryDeadline(getDivUnderSpan("Срок за получаване на оферти или на искания за участие", doc))
                .addEligibleBidLanguage(getDivUnderSpan("Език/ци, на които могат да бъдат", doc))
                .setLots(parseLots(doc))
                .setAwardDecisionDate(getDivUnderSpan("Дата на решението за възлагане на поръчката", doc))
                .setFinalPrice(new ParsedPrice()
                        .setNetAmount(getDivUnderSpan("Обща крайна стойност на поръчката", doc))
                        .setCurrency(getDivUnderSpan("Обща крайна стойност на поръчката", doc))
                );
    }

    /**
     * Parse lots.
     *
     * @param doc document to parse from
     * @return List<ParsedTenderLot> or null
     */
    private static List<ParsedTenderLot> parseLots(final Document doc) {
        final Elements lotFirstLines = JsoupUtils.select("div:contains(ПРИЛОЖЕНИЕ Б: ИНФОРМАЦИЯ ОТНОС" +
                "НО ОБОСОБЕНИТЕ ПОЗИЦИИ) " +
                "~ div:contains(Обособена позиция №)", doc);

        if (lotFirstLines == null || lotFirstLines.isEmpty()) {
            return null;
        }

        final List<Element> lots = new ArrayList<>();

        for (int iterator = 0; iterator < lotFirstLines.size(); iterator++) {
            if ((iterator + 1) != lotFirstLines.size()) {
                lots.add(ParserUtils.getSubsectionOfElements(lotFirstLines.get(iterator),
                        lotFirstLines.get(iterator + 1)));
            } else {
                lots.add(ParserUtils.getSubsectionOfElements(lotFirstLines.get(iterator), null));
            }
        }

        final List<ParsedTenderLot> result = new ArrayList<>();

        int iterator = 0;
        for (Element lot : lots) {
            result.add(new ParsedTenderLot()
                    .setLotNumber(JsoupUtils.selectText("span:containsOwn(Обособена позиция №)", lot))
                    .setPositionOnPage(String.valueOf(iterator++))
                    .setDescription(getDivUnderSpan("Кратко описание", lot))
                    .addCpv(new ParsedCPV()
                            .setIsMain(Boolean.TRUE.toString())
                            .setCode(getDivUnderSpan("CPV", doc)))
                    .addBid(parseBid(doc))
            );
        }

        return result.isEmpty() ? null : result;
    }

    /**
     * Parse bids.
     *
     * @param doc document to parse from
     *
     * @return ParsedBid or null
     */
    private static ParsedBid parseBid(final Document doc) {
        final String bidder = getDivUnderSpan("Наименование и адреси на икономическия оператор, в чиято полза е " +
                "взето решението за възлагане на поръчката", doc);

        return bidder == null ? null
                : new ParsedBid()
                .setIsSubcontracted(getDivUnderSpan("Има възможност поръчката да бъде възложена на под" +
                        "изпълнители", doc))
                .addBidder(new ParsedBody()
                        .setName(bidder));
    }

    /**
     * Parse award criteria.
     *
     * @param doc document to parse from
     *
     * @return List<ParsedAwardCriterion> or null
     */
    private static List<ParsedAwardCriterion> parseAwardCriteria(final Document doc) {
        final Element criteria = JsoupUtils.selectFirst("span:containsOwn(Посочените по-долу критерии:)", doc);

        if (criteria == null || criteria.textNodes().size() > 1) {
            return null;
        }

        final List<ParsedAwardCriterion> result = new ArrayList<>();
        for (int i = 1; criteria.textNodes().size() > i; i++) {
            final String criterion = criteria.textNodes().get(i).text();
            result.add(new ParsedAwardCriterion()
                    .setName(criterion)
                    .setWeight(criterion)
            );
        }

        return null;
    }

    /**
     * Parse buyer.
     *
     * @param doc document to parse from
     * @return ParsedBody
     */
    private static ParsedBody parseBuyer(final Document doc) {
        final Element buyerNameAndEmail = selectFirst("span:containsOwn(Наименование) + div", doc);
        String buyerName = null;
        String buyerEmail = null;
        if (buyerNameAndEmail != null && !buyerNameAndEmail.textNodes().isEmpty()) {
            buyerName = buyerNameAndEmail.textNodes().get(0).text();
            buyerEmail = selectText("a", buyerNameAndEmail);
        }

        return new ParsedBody()
                .setName(buyerName)
                .setEmail(buyerEmail)
                .setBuyerType(getDivUnderSpan("Вид на възлагащия орган и основна дейност или дейности", doc))
                .addMainActivity(getDivUnderSpan("Основна дейност на възложителя", doc));
    }

    /**
     * Get own text of p element using title text which is in nested element.
     *
     * @param title title to be used in selector
     * @param element element to select in
     *
     * @return String or null
     */
    private static String getTextInP(final String title, final Element element) {
        return JsoupUtils.selectOwnText("p:contains(" + title + ")", element);
    }

    /**
     * Get own text of p element using title text which is in nested element.
     *
     * @param title title to be used in selector
     * @param element element to select in
     *
     * @return String or null
     */
    private static String getDivUnderSpan(final String title, final Element element) {
        return JsoupUtils.selectText("span:containsOwn(" + title + ") + div", element);
    }
}
