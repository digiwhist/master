package eu.digiwhist.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Base handler for parsing forms in which information are saved in table HTML element.
 */
abstract class BaseENarocanjeFormInTableHandler {
    // pattern contains only "contains" keyword to find all sections - lot title is in the "td" element
    static final String SECTION_SELECTOR_PATTERN = "div.tab-content > table > tbody > tr:contains(%s)";
    static final String SECTION_FIRST_ELEMENT_SELECTOR_PATTERN = SECTION_SELECTOR_PATTERN + " > *:first-child";

    /**
     * Parses common specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from Form
     */
    static ParsedTender parseCommonAttributes(final ParsedTender tender, final Element form) {
        tender
                .addBuyer(parseBuyer(form));

        assert tender.getPublications().get(0).getIsIncluded().equals(Boolean.toString(true));
        tender.getPublications().get(0)
                .setDispatchDate(ParserUtils.getFromContent(form, "div.tab-content > center > font.naslovmali",
                        "Datum objave:"));

        return tender;
    }

    /**
     * Gets elements representing section (title and content).
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return elements representing section
     */
    static Elements getSections(final String sectionNumber, final Element form) {
        return JsoupUtils.select(String.format(SECTION_SELECTOR_PATTERN, sectionNumber), form);
    }

    /**
     * Gets element representing section (title and content).
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return element representing section
     */
    static Element getSection(final String sectionNumber, final Element form) {
        return JsoupUtils.selectFirst(String.format(SECTION_SELECTOR_PATTERN, sectionNumber), form);
    }

    /**
     * Gets wrapped element representing section (title and content) where each node is separated by <br>.
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return wrapped element representing section where each node is separated by <br>
     */
    static Element getSectionWithSeparatedNodes(final String sectionNumber, final Element form) {
        return ParserUtils.getSubsectionOfNodes(
                JsoupUtils.selectFirst(String.format(SECTION_FIRST_ELEMENT_SELECTOR_PATTERN, sectionNumber), form),
                null);
    }

    /**
     * Gets section content without title. The method suppose that title ends by ":".
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return section content without title
     */
    static String getSectionContent(final String sectionNumber, final Element form) {
        // we suppose that colon is at the end of section title
        return getSectionContent(sectionNumber, form, Collections.singletonList(":"));
    }

    /**
     * Gets section content without title.
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     * @param contentSeparators
     *         separators of title and section content to get just the content. It can be the whole title.
     *
     * @return section content without title
     */
    static String getSectionContent(final String sectionNumber, final Element form,
                                    final List<String> contentSeparators) {
        final Element section = getSection(sectionNumber, form);
        if (section == null) {
            return null;
        }
        final String sectionText = section.text();

        final String contentSeparator = contentSeparators
                .stream()
                .filter(s -> sectionText.contains(s))
                .findFirst()
                .orElse(null);
        assert contentSeparator != null;

        assert sectionText.contains(contentSeparator);
        return sectionText.substring(sectionText.indexOf(contentSeparator) + contentSeparator.length()).trim();
    }

    /**
     * Parse price value from the input text.
     *
     * @param priceText
     *         input text
     *
     * @return price or Null
     */
    static ParsedPrice parsePrice(final String priceText) {
        if (priceText == null) {
            return null;
        }

        String price = priceText;

        final String percentsOfVatText = " % DDV";
        if (price.endsWith(percentsOfVatText)) {
            // Price is with VAT. E.g.
            // - "245954,22 EUR, z 22 % DDV"
            // - "58 620 EUR, z 20 % DDV"

            price = price.substring(0, price.indexOf(percentsOfVatText));

            final String[] priceAndVat = price.split(", z ");
            assert priceAndVat.length == 2;
            assert priceAndVat[0].contains(" ");
            final int amountAndCurrencySeparatorIndex = priceAndVat[0].lastIndexOf(' ');

            return new ParsedPrice()
                    .setAmountWithVat(priceAndVat[0].substring(0, amountAndCurrencySeparatorIndex))
                    .setCurrency(priceAndVat[0].substring(amountAndCurrencySeparatorIndex + 1))
                    .setVat(priceAndVat[1]);
        } else {
            // Price is without VAT. E.g.
            // - "418030,65 EUR, brez DDV"
            // - "50 000 EUR, brez DDV" from https://www.enarocanje.si/Obrazci/?id_obrazec=127713 (II.2.1)
            // - "97056,56 EUR" from https://www.enarocanje.si/Obrazci/?id_obrazec=141257 (III.1.3)
            // We do not parse inaccurate prices. E.g
            // - "do 60.000,00 EUR, brez DDV" from https://www.enarocanje.si/Obrazci/?id_obrazec=126538 (IV.4)

            if (price.matches("^([a-z]|[A-Z]).*")) {
                return null;
            }

            final String withoutVatText = ", brez DDV";
            if (price.endsWith(withoutVatText)) {
                price = price.substring(0, price.indexOf(withoutVatText));
            }

            final int amountAndCurrencySeparatorIndex = price.lastIndexOf(' ');
            return new ParsedPrice()
                    .setNetAmount(price.substring(0, amountAndCurrencySeparatorIndex))
                    .setCurrency(price.substring(amountAndCurrencySeparatorIndex + 1));
        }
    }

    /**
     * Parse tender CPVs from document.
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return List of parsed CPVs
     */
    static List<ParsedCPV> parseTenderCpvs(final String sectionNumber, final Element form) {
        final Element cpvsElement = getSectionWithSeparatedNodes(sectionNumber, form);

        if (cpvsElement == null) {
            return null;
        }

        List<ParsedCPV> tenderCpvs = new ArrayList<>();

        String[] subsectionRows = cpvsElement.html().split("<br>");
        for (String subsectionRow : subsectionRows) {
            subsectionRow = subsectionRow.trim();
            // row containing CPV looks like:
            // - "45000000 (Gradbena dela)"
            // - "36150000 (Pohištvo za šole), dopolnilni besednjak: E018 (Za šole), E160 (Za šole)"
            if (subsectionRow.matches("\\d{8}.*")) {
                assert subsectionRow.contains("(");
                tenderCpvs.add(new ParsedCPV()
                        .setIsMain(Boolean.FALSE.toString())
                        .setCode(subsectionRow.substring(0, subsectionRow.indexOf('(')).trim()));
            }
        }

        return tenderCpvs.isEmpty() ? null : tenderCpvs;
    }

    /**
     * Parse if tender has lots value from document.
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return String or Null
     */
    static String parseIfTenderHasLots(final String sectionNumber, final Element form) {
        String hasLots = getSectionContent(sectionNumber, form, Arrays.asList("Razdelitev na sklope:",
                "RAZDELITEV NA SKLOPE:"));

        if (hasLots.isEmpty()) {
            // e.g. https://www.enarocanje.si/Obrazci/?id_obrazec=116490 in section II.1.5
            return null;
        }

        // the string can be "Da. Ponudbe je treba predložiti za: en ali več sklopov.".
        // See https://www.enarocanje.si/Obrazci/?id_obrazec=118490 in section "II.1.5"
        assert hasLots.contains(".");
        hasLots = hasLots.split("\\p{Punct}")[0];

        return BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(hasLots));
    }

    /**
     * Parses name and address of winning bidder from content of some section.
     *
     * @param nameAndAddress
     *          name and address in string. It will be parsed
     * @param lot
     *          lot to be set
     */
    static void parseNameAndAddressOfWinningBidder(final String nameAndAddress, final ParsedTenderLot lot) {
        if (nameAndAddress.isEmpty()) {
            // e.g. section IV.3 in https://www.enarocanje.si/Obrazci/?id_obrazec=129499
            return;
        }

        final List<String> separators = Arrays.asList(
                "d.o.o.,", "D.O.O.,", "d.o.o. ,", "d. o. o.,", "d.o.o. ",
                "d.d.,", "d.d,",
                "s.p.,", "S.P.,", "s.p. ,",
                ",");
        final String separator = separators
                .stream()
                .filter(s -> nameAndAddress.contains(s))
                .findFirst()
                .orElse(null);
        assert separator != null;

        assert nameAndAddress.contains(separator);

        if (lot.getBids() == null) {
            lot
                    .addBid(new ParsedBid()
                            .setIsWinning(Boolean.TRUE.toString()));
        }

        final int addressStartIndex = nameAndAddress.indexOf(separator) + separator.length();
        lot.getBids().get(0)
                .addBidder(new ParsedBody()
                        .setName(nameAndAddress.substring(0, addressStartIndex))
                        .setAddress(new ParsedAddress()
                                .setRawAddress(nameAndAddress.substring(addressStartIndex))));
    }

    /**
     * Parses buyer info.
     *
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return parsed buyer info
     */
    private static ParsedBody parseBuyer(final Element form) {
        String sectionI1Content = getSectionContent("I.1", form, Arrays.asList(
                "Ime, naslovi in kontaktna(-e) točka(-e):",
                "IME, NASLOVI IN KONTAKTNE TOČKE:",
                "IME, NASLOVI IN KONTAKTNA(-E) TOČKA(-E):",
                "IME, NASLOVI IN KONTAKTNE TOČKE",
                "IME, NASLOVI IN KONTAKTNA(-E) TOČKA(-E)"));

        if (sectionI1Content == null) {
            return null;
        }

        // get phone and delete useless content
        final String phoneTitle = "Tel.";
        String phone = null;
        if (sectionI1Content.contains(phoneTitle)) {
            final int phoneIndex = sectionI1Content.indexOf(phoneTitle) + phoneTitle.length();
            phone = sectionI1Content.substring(phoneIndex, sectionI1Content.indexOf('.', phoneIndex)).trim();
            // delete phone and the rest information from section I.1 content. We do not want it
            sectionI1Content = sectionI1Content.substring(0, phoneIndex - phoneTitle.length());
        } else {
            // the content does not contain phone. Never mind. But we want to cut the useless end of section I.1 content
            // to have just raw address and contact point in the content

            final String faxTitle = "Telefaks";
            if (sectionI1Content.contains(faxTitle)) {
                sectionI1Content = sectionI1Content.substring(0, sectionI1Content.indexOf(faxTitle));
            }

            final String emailTitle = "E-pošta";
            if (sectionI1Content.contains(emailTitle)) {
                sectionI1Content = sectionI1Content.substring(0, sectionI1Content.indexOf(emailTitle));
            }

            final String urlTitle = "Internetni naslov naročnika";
            if (sectionI1Content.contains(urlTitle)) {
                sectionI1Content = sectionI1Content.substring(0, sectionI1Content.indexOf(urlTitle));
            }
        }

        // get rawAddress and contact point
        final String contactPointTitle = "Kontakt:";
        final String[] rawAddressAndContactPoint = sectionI1Content.split(contactPointTitle);
        assert rawAddressAndContactPoint.length <= 2;

        return new ParsedBody()
                .setAddress(new ParsedAddress()
                        .setRawAddress(rawAddressAndContactPoint[0].trim()))
                .setContactPoint(rawAddressAndContactPoint.length == 2 ? rawAddressAndContactPoint[1].trim() : null)
                .setPhone(phone);
    }

}
