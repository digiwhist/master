package eu.datlab.worker.bg.parsed;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedFunding;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static eu.datlab.dataaccess.dto.codetables.PublicationSources.BG_AOP;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectFirst;
import static eu.dl.worker.utils.jsoup.JsoupUtils.selectText;
import static eu.datlab.worker.bg.parsed.AOPParserUtils.parseBoolean;


/**
 * Created by michalriha on 12/07/2017.
 */
public final class AOPTenderSecondFormHandler {
    private static final Logger logger = LoggerFactory.getLogger(AOPTenderSecondFormHandler.class);

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
        String procedureType = getDivUnderSpan("Вид процедура", doc);

        ParsedPrice finalPrice = parsePrice(getDivUnderSpan("Обща крайна стойност на поръчката", doc));
        if (finalPrice == null) {
            Element node = JsoupUtils.selectFirst("div:has(span:contains(Обща стойност на договора/ите)) + div", doc);
            finalPrice = parsePrice(getDivUnderSpan("Стойност", node));
        }

        ParsedFunding parsedFunding = null;
        String funding = getDivUnderSpan("Поръчката е свързана с проект и/или програма, финансирани от фондове на ЕС", doc);
        if (funding != null && !funding.isEmpty()) {
            parsedFunding = new ParsedFunding()
                .setIsEuFund(parseBoolean(funding))
                .setProgramme(funding.equals("НЕ") ? null : funding);
        }

        ParsedAddress addrOfImpl = null;
        String nuts = getDivUnderSpan("Обект на поръчката и място на изпълнение на строителството, доставката или услугата", doc);
        if (nuts != null) {
            Matcher m = Pattern.compile("Код NUTS:? (?<nuts>[A-Z]{2}[0-9]+)").matcher(nuts);
            if (m.find()) {
                addrOfImpl = new ParsedAddress().addNuts(m.group("nuts"));
            }
        }

        return new ParsedTender()
            .setBuyerAssignedId(selectText("body > font", doc))
            .addPublication(new ParsedPublication()
                .setSourceFormType(sourceFormType)
                .setIsIncluded(true)
                .setSource(BG_AOP)
                .setHumanReadableUrl(url)
                .setPublicationDate(getDivUnderSpan(doc, null,
                    "Дата на изпращане на настоящото обявление",
                    "Дата на изпращане на настоящата информация за сключен договор")))
            .addBuyer(parseBuyer(doc))
            .setAdditionalInfo(getTextInP("Адреси и места за контакти, от които може да се получи допълнителна информация", doc))
            .setIsOnBehalfOf(parseBoolean(getDivUnderSpan(doc, null,
                "Възлагащият орган извършва покупка от името на други възлагащи органи",
                "Възложителят възлага обществена/и поръчка/и от името на друг/и възложител/и")))
            .setTitle(getDivUnderSpan(doc, null,
                "Заглавие на поръчката, предоставено от възлагащия орган",
                "Наименование на поръчката, дадено от възложителя"))
            .setDescription(getDivUnderSpan("Кратко описание на поръчката или покупката/покупките", doc))
            .addCpv(new ParsedCPV()
                .setIsMain(Boolean.TRUE.toString())
                .setCode(getDivUnderSpan(doc, "span.txcpv", "CPV")))
            .setIsCoveredByGpa(parseBoolean(getDivUnderSpan("GPA", doc)))
            .setHasLots(parseBoolean(getDivUnderSpan("Разделяне на обособени позиции", doc)))
            .setAreVariantsAccepted(parseBoolean(getDivUnderSpan("Ще бъдат приемани варианти", doc)))
            .setEstimatedPrice(parsePrice(getDivUnderSpan("Прогнозна стойност без ДДС", doc)))
            .setHasOptions(parseBoolean(getDivUnderSpan("Опции", doc)))
            .setEstimatedDurationInMonths(getDivUnderSpan("Продължителност в месеци", doc))
            .setDeposits(getDivUnderSpan("Изискуеми депозити и гаранции", doc))
            .setPersonalRequirements(getDivUnderSpan("Лично състояние към икономическите оператори", doc))
            .setEconomicRequirements(getDivUnderSpan("Икономически и финансови възможности", doc))
            .setTechnicalRequirements(getDivUnderSpan("Технически възможности", doc))
            .setProcedureType(procedureType)
            .setNationalProcedureType(procedureType)
            .setSelectionMethod(getDivUnderSpan("Критерии за възлагане", doc))
            .setAwardCriteria(parseAwardCriteria(doc))
            .setIsElectronicAuction(parseBoolean(getDivUnderSpan("Ще се използва електронен търг", doc)))
            .setDocumentsDeadline(parseBoolean(getDivUnderSpan(doc, null,
                "Срок за получаване на искания за документи или за достъп до документи",
                "Срок за получаване на документация за участие")))
            .setDocumentsPayable(getDivUnderSpan("Платими документи", doc))
            .setEnquiryDeadline(getDivUnderSpan("Срок за получаване на оферти или на искания за участие", doc))
            .addEligibleBidLanguage(getDivUnderSpan("Език/ци, на които могат да бъдат", doc))
            .setLots(parseLots(doc))
            .setAwardDecisionDate(getDivUnderSpan("Дата на решението за възлагане на поръчката", doc))
            .setFinalPrice(finalPrice)
            .setSupplyType(JsoupUtils.selectText("div.DocumentBody div.stdoc:eq(1)", doc))
            .setBidDeadline(getDivUnderSpan(doc, null,
                "Срок за получаване на документация за участие",
                "Срок за получаване на оферти или на искания за участие",
                "Срок за получаване на оферти или заявления за участие"))
            .setEstimatedDurationInMonths(getDivUnderSpan(doc, null, "Срок на изпълнение в месеци", "Срок в месеци"))
            .addFunding(parsedFunding)
            .setSelectionMethod(getDivUnderSpan(doc, null, "Критерии за оценка на офертите", "Критерии за възлагане"))
            .setFurtherInformationProvider(parseBody(
                selectFirst("p:has(b:containsOwn(Адреси и места за контакти, от които може да се получи допълнителна информация)) + div",
                    doc)))
            .setSpecificationsProvider(parseBody(
                selectFirst("p:has(b:containsOwn(Адреси и места за контакти, от които може да се получат спецификациите и допълнителни" +
                    " документи)) + div", doc)))
            .setBidsRecipient(parseBody(
                selectFirst("p:has(b:containsOwn(Адреси и места за контакти, на които трябва да бъдат изпратени офертите/заявленията за" +
                    " участие)) + div", doc)))
            .setAddressOfImplementation(addrOfImpl);
    }

    /**
     * Parse lots.
     *
     * @param doc document to parse from
     * @return List<ParsedTenderLot> or null
     */
    private static List<ParsedTenderLot> parseLots(final Document doc) {
        String lotFirstLineRegex = "(?i)(Договор.+/|Обособена позиция.+/)";
        final Elements lotFirstLines = JsoupUtils.select(
            "div:contains(ПРИЛОЖЕНИЕ Б: ИНФОРМАЦИЯ ОТНОСНО ОБОСОБЕНИТЕ ПОЗИЦИИ) ~ div:matches(" + lotFirstLineRegex + ")," +
            "div:matches(РАЗДЕЛ V: (СКЛЮЧВАНЕ НА ДОГОВОР)|(ВЪЗЛАГАНЕ НА ПОРЪЧКАТА)) ~ div:matches(" + lotFirstLineRegex + ")", doc);
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

        Pattern p = Pattern.compile("(?:((Поръчка \\(Договор\\))|Договор|ДОГОВОР) №:? ?(?<contract>[^/]+))?" +
            "(?:(/ )?Обособена позиция №:? ?(?<lot>[^/]+)?)?" +
            "(?:(/ )?(Заглавие|(Заглавие на обособената позиция)|Наименование):? ?(?<name>.+))?", Pattern.CASE_INSENSITIVE);

        for (Element e : lots) {
            ParsedTenderLot lot = new ParsedTenderLot()
                .setPositionOnPage(String.valueOf(iterator++))
                .setDescription(getDivUnderSpan("Кратко описание", e))
                .addCpv(new ParsedCPV()
                    .setIsMain(Boolean.TRUE.toString())
                    .setCode(getDivUnderSpan(e, "span.txcpv", "CPV")))
                .addBid(parseBid(e))
                .setBidsCount(getDivUnderSpan("Брой на получените оферти", e))
                .setEstimatedPrice(parsePrice(getDivUnderSpan("Прогнозна стойност без ДДС", e)))
                .setAwardDecisionDate(getDivUnderSpan("Дата на решението за възлагане на поръчката", e));

            Matcher m = p.matcher(JsoupUtils.selectText("span:matchesOwn(Договор|ДОГОВОР)," +
                " span:containsOwn(Обособена позиция)", e, true));
            if (m.find()) {
                lot.setLotNumber(m.group("lot"))
                    .setContractNumber(m.group("contract"))
                    .setTitle(m.group("name"));
            }

            result.add(lot);
        }

        return result.isEmpty() ? null : result;
    }

    /**
     * Parse bids.
     *
     * @param node node to parse from
     *
     * @return ParsedBid or null
     */
    private static ParsedBid parseBid(final Element node) {
        final ParsedBody bidder = parseBody(selectFirst(
            "span:containsOwn(Наименование и адреси на икономическия оператор, в чиято полза е взето решението за възлагане на поръчката)" +
            " + div, span:containsOwn(Наименование и адрес на изпълнителя) + div", node));

        if (bidder == null) {
            return null;
        }

        return new ParsedBid()
            .setIsWinning(String.valueOf(true))
            .setIsSubcontracted(parseBoolean(getDivUnderSpan(node, null,
                "При изпълнение на договора ще участват подизпълнители",
                "Има възможност поръчката да бъде възложена на подизпълнители")))
            .addBidder(bidder)
            .setPrice(parsePrice(getDivUnderSpan(node, null, "Обща крайна стойност на поръчката", "Крайна обща стойност на договора")));
    }

    /**
     * Parse award criteria.
     *
     * @param doc document to parse from
     *
     * @return List<ParsedAwardCriterion> or null
     */
    private static List<ParsedAwardCriterion> parseAwardCriteria(final Document doc) {
        final Element criteria = selectFirst("span:containsOwn(Посочените по-долу критерии:)", doc);

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
        Elements nodes = JsoupUtils.select(doc,
            "span:matchesOwn(Наименование, адреси и (лица|място/места) за контакт) + div",
            "span:matchesOwn(Наименование и адрес) + div");
        if (nodes == null || nodes.isEmpty()) {
            return null;
        }

        return parseBody(nodes.first())
            .setBuyerType(getDivUnderSpan(doc, null,
                "Вид на възлагащия орган и основна дейност или дейности",
                "Вид на възложителя и основна дейност/и"))
            .addMainActivity(getDivUnderSpan(doc, null,
                "Основна дейност на възложителя",
                "Основна/и дейност/и на възложителя, свързана/и с"));
    }

    /**
     * Parses body from the given node.
     *
     * @param node
     *      node to be parsed
     * @return parsed body or null
     */
    private static ParsedBody parseBody(final Element node) {
        if (node == null) {
            return null;
        }

        String body = JsoupUtils.selectText("p.addr:eq(0)", node);

        Matcher m = Pattern.compile("(?<name>[^,]+?)(?<id>\\d+)?,(?<addr>[^~]+)(~Тел\\.: (?<phone>[^~]+))?(~E-mail: (?<email>[^~]+))?" +
            "(~Факс: (?<fax>[^~]+))?")
            .matcher(body.replaceAll("((, )(Тел\\.:|E-mail:|Факс:))", "~$3"));

        if (m.find()) {
            return new ParsedBody()
                .setName(m.group("name"))
                .setEmail(m.group("email"))
                .setPhone(m.group("phone"))
                .setAddress(new ParsedAddress().setRawAddress(m.group("addr")))
                .setContactPoint(node.ownText().replace("Място/места за контакт:", ""))
                .addBodyId(new BodyIdentifier()
                    .setId(m.group("id")).setScope(BodyIdentifier.Scope.BG).setType(BodyIdentifier.Type.ORGANIZATION_ID));
        } else {
            logger.error("Body string '{}' has an unexpected format", body);
            throw new UnrecoverableException("Body string has an unexpected format");
        }
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
     * Get text of sibling div element of span that contains own title text.
     *
     * @param title title to be used in selector
     * @param element element to select in
     *
     * @return String or null
     */
    private static String getDivUnderSpan(final String title, final Element element) {
        return getDivUnderSpan(element, null, title);
    }

    /**
     * Get text of sibling div element of span that contains own title text. If the valueSelector is not empty, it applies this selector on
     * sibling div and returns text of this selection.
     *
     * @param element element to select in
     * @param valueSelector selector applied on sibling div
     * @param title set of titles to be used in selectors
     *
     * @return String or null
     */
    private static String getDivUnderSpan(final Element element, final String valueSelector, final String... title) {
        String[] selectors = new String[title.length];
        for (int i = 0, len = title.length; i < len; i++) {
            selectors[i] = "span:containsOwn(" + title[i] + ") + div";
        }

        Elements nodes = JsoupUtils.select(element, selectors);
        if (nodes == null) {
            return null;
        } else if (valueSelector == null || valueSelector.isEmpty()) {
            return nodes.first().text();
        }

        return JsoupUtils.selectText(valueSelector, nodes.first());
    }

    /**
     * @param priceAndCurrency
     *      string that contains price and currency (eg. 39180 BGN, 87236 BGN без ДДС)
     * @return parsed price or null
     */
    private static ParsedPrice parsePrice(final String priceAndCurrency) {
        if (priceAndCurrency == null) {
            return null;
        }

        String[] values = priceAndCurrency
            .replace("без ДДС", "")
            .replaceAll("Стойност ?:?", "")
            .trim()
            .split(" ", 2);

        return new ParsedPrice().setNetAmount(values[0]).setCurrency(values.length > 1 ? values[1] : null);
    }
}
