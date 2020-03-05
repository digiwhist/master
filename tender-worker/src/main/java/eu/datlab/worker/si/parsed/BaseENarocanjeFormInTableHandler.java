package eu.datlab.worker.si.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.jsoup.nodes.Element;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Base handler for parsing forms in which information are saved in table HTML element.
 */
abstract class BaseENarocanjeFormInTableHandler {
    /**
     * Parses common attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from form
     */
    static ParsedTender parseCommonFormInTableAttributes(final ParsedTender tender, final Element form) {
        final Element sectionI12 = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("I.1.2", form);
        final Element sectionII3 = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("II.3", form);
        final Element sectionIV33 = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("IV.3.3", form);

        tender
                .addBuyer(parseBuyer(form))
                .setAddressOfImplementation(new ParsedAddress()
                    .setRawAddress(ParserUtils.getFromContent(sectionI12, null, " Glavna lokacija ali mesto gradnje:"))
                    .addNuts(StringUtils.removeDotsAtTheEnd(ParserUtils.getFromContent(sectionI12, null,
                        " Sifra NUTS:"))))
                .setIsCoveredByGpa(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                        ENarocanjeTenderFormInTableUtils.getSectionContent("II.1.7", form, Arrays.asList(
                                "Naročilo je vključeno v Sporazum o vladnih naročilih (GPA)")))))
                .setEstimatedDurationInDays(parseTenderEstimatedDurationInDays(sectionII3))
                .setPersonalRequirements(ENarocanjeTenderFormInTableUtils.getSectionContent("III.2.1", form,
                        Arrays.asList(
                                "Osebni status gospodarskih subjektov, vključno z zahtevami v zvezi z vpisom v " +
                                        "register poklicev ali trgovski register")))
                .setEconomicRequirements(ENarocanjeTenderFormInTableUtils.getSectionContent("III.2.2", form,
                        Arrays.asList("Poslovna in finančna sposobnost")))
                .setTechnicalRequirements(ENarocanjeTenderFormInTableUtils.getSectionContent("III.2.3", form,
                        Arrays.asList("Tehnična sposobnost")))
                .setDocumentsPayable(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                        ParserUtils.getFromContent(sectionIV33, null, " Dokumentacija se plačuje:"))))
                .setDocumentsPrice(parseDocumentsPrice(sectionIV33))
                .setIsOnBehalfOf(parseIsTenderOnBehalfOfSomeone(form));

        assert tender.getPublications().get(0).getIsIncluded().equals(Boolean.toString(true));
        tender.getPublications().get(0)
                .setPublicationDate(ParserUtils.getFromContent(form, "div.tab-content > center > font.naslovmali",
                        "Datum objave:"));

        return tender;
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
            return amountAndCurrencySeparatorIndex < 0 ? null : new ParsedPrice()
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
        final Element cpvsElement = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes(sectionNumber, form);

        if (cpvsElement == null) {
            return null;
        }

        List<ParsedCPV> tenderCpvs = new ArrayList<>();

        List<String> subsectionRows = ENarocanjeTenderFormUtils.splitByBR(cpvsElement);
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
        String hasLots = ENarocanjeTenderFormInTableUtils.getSectionContent(sectionNumber, form, Arrays.asList(
                "Razdelitev na sklope:",
                "Razdelitev na sklope",
                "RAZDELITEV NA SKLOPE:"));

        if (hasLots == null) {
            // it can happen when the form contains no information. See
            // e.g. http://www.enarocanje.si/Obrazci/?id_obrazec=81539
            return null;
        }
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
        if (lot.getBids() == null) {
            lot.addBid(new ParsedBid().setIsWinning(Boolean.TRUE.toString()));
        }

        lot.getBids().get(0)
            .addBidder(ENarocanjeTenderFormUtils.parseNameAndAddressBody(nameAndAddress, null));
    }

    /**
     * Parses supply type.
     *
     * @param sectionNumber
     *          number of section in title
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return String or null
     */
    static String parseSupplyType(final String sectionNumber, final Element form) {
        final Element section = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes(sectionNumber, form);

        if (section == null) {
            return null;
        }

        /* The supply type should be the first information in the content of section. Index of the array created by
           split by <br> is 6. The example below is from https://www.enarocanje.si/Obrazci/?id_obrazec=185848
        <wrapper>
         <script>_l('%0d%0a%3c%74d v%61lig%6e%3d%74%6fp%3e%0d%0a%3cf%6f%6e%74 cl%61ss%3d%22vpr%61s%61%6ej%65%22%3e')
         </script>
         <br>II.1.2)
         <br>
         <script>_l('%26%6ebsp%3b%3c%2ff%6f%6e%74%3e%0d%0a%3c%2f%74d%3e%0d%0a%3c%74d
         v%61lig%6e%3d%74%6fp%3e%0d%0a%3cf%6f%6e%74 cl%61ss%3d%22vpr%61s%61%6ej%65%22%3e')</script>
         <br>Vrsta naročila in mesto gradnje, kraj dobave ali izvedbe
         <br>
         <script>_l('%3c%2ff%6f%6e%74%3e')</script>
         <br>
         <script>_l('%0d%0a%3c%2f%74d%3e%0d%0a%3c%2f%74r%3e%0d%0a%3c%74r%3e%0d%0a%3c%74d%3e%3c%2f%74d%3e%0d%0a%3c%74d
         v%61lig%6e%3d%74%6fp%3e')</script>
         <br> Blago.
         <br>Nakup.
         <br> Glavni kraj dobave: Prostori naročnika
         <br> Sifra NUTS: SI.
         <br>
         <br>
         <br>
        </wrapper>
        */
        return StringUtils.removeDotsAtTheEnd(ParserUtils.getFromContent(section, null, 6));
    }

    /**
     * Parse tender award criterion list from publication element.
     *
     * @param section
     *         section element to be parsed
     *
     * @return award criterion list or Null
     */
    static List<ParsedAwardCriterion> parseTenderAwardCriteria(final Element section) {
        // each award criteria starts with order number. See
        // https://www.enarocanje.si/Obrazci/?id_obrazec=33117

        List<ParsedAwardCriterion> awardCriteria = new ArrayList<>();

        Integer orderNumber = 1;
        final String beginOfAwardCriterionPattern = " %s.";
        String awardCriterion = ParserUtils.getFromContent(section, null,
                String.format(beginOfAwardCriterionPattern, orderNumber.toString()));
        while (awardCriterion != null) {
            // e.g.:
            // - "cena. Pondeniranje: 85"
            // - "Merila za 6. skupino:. Pondeniranje:", see https://www.enarocanje.si/Obrazci/?id_obrazec=33099
            String[] nameAndWeight = awardCriterion.split("Pondeniranje:");
            assert nameAndWeight.length <= 2;
            awardCriteria.add(new ParsedAwardCriterion()
                    .setName(StringUtils.removeDotsAtTheEnd(nameAndWeight[0]))
                    .setWeight(nameAndWeight.length == 2 ? StringUtils.removeDotsAtTheEnd(nameAndWeight[1]) : null));

            orderNumber++;
            awardCriterion = ParserUtils.getFromContent(section, null,
                    String.format(beginOfAwardCriterionPattern, orderNumber.toString()));
        }

        return awardCriteria;
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
        String sectionI1Content = ENarocanjeTenderFormInTableUtils.getSectionContent("I.1", form, Arrays.asList(
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

        // get buyer name, rawAddress and contact point
        final String contactPointTitle = "Kontakt:";
        final String[] nameRawAddressAndContactPoint = sectionI1Content.split(contactPointTitle);
        assert nameRawAddressAndContactPoint.length <= 2;
        String nameAndRawAddress = nameRawAddressAndContactPoint[0].trim();
        if (nameAndRawAddress.endsWith(",")) {
            nameAndRawAddress = nameAndRawAddress.substring(0, nameAndRawAddress.length() - 1);
        }
        // last comma is separator. Example of the string is "JAVNI HOLDING Ljubljana, d.o.o., Verovškova ulica 70"
        final int nameAndRawAddressSeparatorIndex = nameAndRawAddress.lastIndexOf(',');
        assert nameAndRawAddressSeparatorIndex != -1;
        final String name = nameAndRawAddress.substring(0, nameAndRawAddressSeparatorIndex).trim();        
        final String contactPoint = nameRawAddressAndContactPoint.length == 2
                ? nameRawAddressAndContactPoint[1].trim()
                : null;
        String rawAddress = nameAndRawAddress.substring(nameAndRawAddressSeparatorIndex + 1).trim();
        if (contactPoint != null) {
            rawAddress += "," + contactPoint;
        }

        List<ParsedBody> buyers = Arrays.asList(new ParsedBody()
            .setName(name)
            .setAddress(new ParsedAddress().setRawAddress(rawAddress))
            .setContactPoint(contactPoint)
            .setPhone(phone)
            .setMainActivities(parseBuyerMainActivities(form)));

        buyers = ENarocanjeTenderFormInTableUtils.parseBuyerType(buyers, form);

        return buyers.get(0);
    }

    /**
     * Parse tender estimated duration in days value from section II.3.
     *
     * @param sectionII3
     *         section II.3 to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderEstimatedDurationInDays(final Element sectionII3) {
        String numberOfDays = ParserUtils.getFromContent(sectionII3, null,
                " " + ENarocanjeTenderFormUtils.DURATION_IN_DAYS_TITLE);
        if (numberOfDays == null) {
            return null;
        }
        assert numberOfDays.endsWith(ENarocanjeTenderFormUtils.DURATION_IN_DAYS_SUFFIX);
        return numberOfDays.substring(0, numberOfDays.indexOf(ENarocanjeTenderFormUtils.DURATION_IN_DAYS_SUFFIX))
                .trim();
    }

    /**
     * Parses documents price.
     *
     * @param sectionIV33
     *          section IV.3.3 to be parsed
     *
     * @return documents price or null
     */
    private static ParsedPrice parseDocumentsPrice(final Element sectionIV33) {
        final String priceAndCurrencyString = ParserUtils.getFromContent(sectionIV33, null, " Cena:");
        if (priceAndCurrencyString == null) {
            return null;
        }
        assert priceAndCurrencyString.contains(" ")
                && priceAndCurrencyString.indexOf(' ') == priceAndCurrencyString.lastIndexOf(' ');
        final String[] priceAndCurrency = priceAndCurrencyString.split(" ");
        return new ParsedPrice()
                .setNetAmount(priceAndCurrency[0])
                .setCurrency(priceAndCurrency[1]);
    }

    /**
     * Returns provided main activity of the buyer.
     *
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return buyer main activity or null
     */
    private static List<String> parseBuyerMainActivities(final Element form) {
        List<String> activities = new ArrayList<>();

        // section I.2 or I.3 (same title)
        // maybe it is content of the section I.2. See
        // https://www.enarocanje.si/Obrazci/?id_obrazec=83105
        Element activitiesNode = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("GLAVNA PODROČJA"
            + " DEJAVNOSTI", form);

        if (activitiesNode != null) {
            activities = ENarocanjeTenderFormUtils.splitByBR(activitiesNode).stream()
                // skip useless rows
                .filter(n -> !n.contains("<script>") && !n.contains("GLAVNA PODROČJA DEJAVNOSTI")
                    && !n.startsWith("I.") && !n.trim().isEmpty())
                .map(n -> n.trim())
                .collect(Collectors.toList());
        }

        if (activities.isEmpty()) {
            // maybe it is in the second row in I.2. See
            // https://www.enarocanje.si/Obrazci/?id_obrazec=37402
            Element section = ENarocanjeTenderFormInTableUtils.getSection("^I.2\\)", form);
            if (!section.text().toLowerCase().contains("i.2) vrsta naročnika in glavna(-o) področja(-e) dejavnosti:")) {
                // https://www.enarocanje.si/Obrazci/?id_obrazec=10766
                return null;
            }
            section = section.nextElementSibling().nextElementSibling();
            activities.add(section.text());
        }

        return activities.isEmpty() ? null : activities;
    }

    /**
     * Parse if tender is made on behalf of someone value from document.
     *
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return String or Null
     */
    private static String parseIsTenderOnBehalfOfSomeone(final Element form) {
        final Element sectionI2 = ENarocanjeTenderFormInTableUtils.getSection("I.2", form);
        if (sectionI2 == null) {
            // it can happen when the form contains no information. See
            // e.g. http://www.enarocanje.si/Obrazci/?id_obrazec=81539
            return null;
        }

        final Element isTenderOnBehalfOfNode = JsoupUtils.getNthSibling(sectionI2, 4);
        if (isTenderOnBehalfOfNode == null) {
            return null;
        }

        final String isTenderOnBehalfOfSomeoneText = isTenderOnBehalfOfNode.text();
        final String isTenderOnBehalfOfSomeoneTitle = "Naročnik izvaja postopek v imenu drugih naročnikov:";

        return isTenderOnBehalfOfSomeoneText.startsWith(isTenderOnBehalfOfSomeoneTitle)
                ? BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(StringUtils.removeDotsAtTheEnd(
                        isTenderOnBehalfOfSomeoneText.substring(isTenderOnBehalfOfSomeoneTitle.length()))))
                : null;
    }
}
