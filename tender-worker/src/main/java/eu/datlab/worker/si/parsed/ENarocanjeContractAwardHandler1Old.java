package eu.datlab.worker.si.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.parsed.utils.ParserUtils;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;

import static eu.datlab.worker.si.parsed.BaseENarocanjeFormInTableHandler.parsePrice;

import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang.BooleanUtils;
import org.jsoup.nodes.Attributes;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.TextNode;
import org.jsoup.parser.Tag;
import org.jsoup.select.Elements;

/**
 * Handler for parsing old forms "EU 3 - SL".
 */
final class ENarocanjeContractAwardHandler1Old extends BaseENarocanjeContractAwardInTableHandler {
    /**
     * Private constructor to make this class static.
     */
    private ENarocanjeContractAwardHandler1Old() {}

    /**
     * Parses form specific attributes and updates the passed tender.
     *
     * @param tender
     *         tender to be updated with parsed data
     * @param form
     *         parsed document for the source HTML page (parsed form)
     *
     * @return updated tender object with data parsed from form
     */
    public static ParsedTender parse(final ParsedTender tender, final Element form) {
        parseCommonContractAwardAttributes(tender, form);


        // supply type
        Element node = JsoupUtils.selectFirst("div.tab-content > center > font.naslov", form);
        if (node != null && node.childNodeSize() > 0) {
            tender.setSupplyType(
                // find first text node, its value is supplyType
                node.childNodes().stream()
                    .filter(n -> n instanceof TextNode)
                    .map(n -> ((TextNode) n).text())
                    .findFirst().orElse(null));
        }

        // cpv
        Element cpvNode = ENarocanjeTenderFormInTableUtils.getSection("II.1.5", form);
        if (cpvNode != null && cpvNode.text().contains("(CPV):")) {
            String code = cpvNode.text().split("\\(CPV\\): ")[1].split(" ")[0];
            tender.addCpv(new ParsedCPV().setCode(code));
        }
        
        tender
            .setDescription(ENarocanjeTenderFormInTableUtils.getSectionContent("Kratek opis naročila ali nabave",
                form))
            .setIsCoveredByGpa(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                ENarocanjeTenderFormInTableUtils.getSectionContent("II.1.6", form, Arrays.asList(
                    "Naročilo je vključeno v Sporazum o vladnih naročilih (GPA)")))))
            .setFinalPrice(parsePrice(ParserUtils.getFromContent(
                ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("II.2.1", form), null, " Vrednost:")))
            .setIsElectronicAuction(BooleanUtils.toStringTrueFalse(ENarocanjeTenderFormUtils.meansYes(
                ENarocanjeTenderFormInTableUtils.getSectionContent("IV.2.2", form, Arrays.asList(
                    "Uporabljena je bila elektronska dražba")))))
            .setSelectionMethod(StringUtils.removeDotsAtTheEnd(ParserUtils.getFromContent(
                ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("IV.2.1", form), null, 6)));
        
            // procedure and nationalProceduType
            Element sectionIV11 = ENarocanjeTenderFormInTableUtils.getSection("IV.1.1", form);
            if (sectionIV11 != null) {
                String procedureType = StringUtils.removeDotsAtTheEnd(sectionIV11.text()
                    .replace("IV.1.1) Vrsta postopka ", ""));
                
                tender.setNationalProcedureType(procedureType).setProcedureType(procedureType);
            }

        // previous publication
        Element sectionIV32 = ENarocanjeTenderFormInTableUtils.getSection("IV.3.2", form);
        if (sectionIV32 != null) {
            Matcher m = Pattern.compile("(?<=z dne )(?<date>\\d{1,2}\\. \\d{1,2}\\. \\d{4})")
                .matcher(sectionIV32.text());
            String publicationDate = m.find() ? m.group("date") : null;

            String href = JsoupUtils.selectAttribute("a", "href", sectionIV32);
            String url = href != null ? PublicationSources.SI_ENAROCANJE + "/Obrazci/" + href : null;

            tender.addPublication(new ParsedPublication()
                .setIsIncluded(false)
                .setHumanReadableUrl(url)
                .setSourceId(JsoupUtils.selectText("a", sectionIV32))
                .setSource(PublicationSources.SI_ENAROCANJE)
                .setPublicationDate(publicationDate));
        }

        // lots
        Elements lotHeadNodes = JsoupUtils.select("tr:contains(ODDELEK V: ODDAJA NAROČILA)", form);
        Elements lotNodes = new Elements();
        if (lotHeadNodes != null) {
            lotHeadNodes.stream().forEach(n -> {
                Element tbl = new Element(Tag.valueOf("table"), "");
                tbl.appendChild(new Element(Tag.valueOf("tbody"), ""));

                Attributes attrs = new Attributes();
                attrs.put("class", "tab-content");
                Element container = new Element(Tag.valueOf("div"), "", attrs);
                container.appendChild(tbl);

                Element row = n;
                do {
                    row = row.nextElementSibling();
                    if (row == null || row.text().contains("ODDELEK V:") || row.text().contains("ODDELEK VI:")) {
                        lotNodes.add(container);
                        break;
                    }

                    tbl.child(0).appendChild(row.clone());
                } while (true);
            });
        }

        lotNodes.forEach(n -> {
            Element priceNode = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("^V.4\\)", n);

            tender.addLot(new ParsedTenderLot()
                .setContractNumber(ENarocanjeTenderFormInTableUtils.getSectionContent("ŠT. NAROČILA:", n,
                    Arrays.asList("ŠT. NAROČILA:")))
                .setAwardDecisionDate(ENarocanjeTenderFormInTableUtils.getSectionContent("^V.1\\)", n, Arrays.asList(
                    "DATUM ODDAJE NAROČILA:")))
                .setBidsCount(ENarocanjeTenderFormInTableUtils.getSectionContent("^V.2\\)", n, Arrays.asList(
                    "ŠTEVILO PREJETIH PONUDB")))
                .setEstimatedPrice(parsePrice(ParserUtils.getFromContent(priceNode, null, " Začetna skupna ocenjena"
                    + " vrednost naročila:")))
                .addBid(new ParsedBid()
                    .setIsWinning(Boolean.TRUE.toString())
                    .addBidder(ENarocanjeTenderFormUtils.parseNameAndAddressBody(
                        ENarocanjeTenderFormInTableUtils.getSectionContent("^V.3\\)", n,
                            Arrays.asList("IME IN NASLOV GOSPODARSKEGA SUBJEKTA, KI MU JE BILO NAROČILO ODDANO:")),
                        null))
                    .setPrice(parsePrice(ParserUtils.getFromContent(priceNode, null, " Vrednost:")))));
        });

        // award criteria
        Element criteriaNode = ENarocanjeTenderFormInTableUtils.getSectionWithSeparatedNodes("^IV.2.1\\)", form);
        if (criteriaNode != null) {
            Pattern regex = Pattern.compile("\\s*[0-9]+\\. (?<name>[^\\.]+)\\. Pondeniranje: (?<weight>[0-9\\.]+)");

            List<ParsedAwardCriterion> criteria = criteriaNode.childNodes().stream()
                .filter(n -> n.toString().contains("Pondeniranje:"))
                .flatMap(n -> {
                    Matcher m = regex.matcher(n.toString());
                    if (m.find()) {
                        return Stream.of(new ParsedAwardCriterion()
                            .setName(m.group("name"))
                            .setWeight(m.group("weight"))
                            .setIsPriceRelated((m.group("name").equalsIgnoreCase("cena") ? Boolean.TRUE : Boolean.FALSE)
                                .toString()));
                    }
                    
                    return Stream.empty();
                }).collect(Collectors.toList());

            if (!criteria.isEmpty()) {
                tender.setAwardCriteria(criteria);
            }
        }
//    Ekonomsko najugodnejša ponudba glede na:<br>
//    1. cena. Pondeniranje: 95<br>
//    2. ISO 9001. Pondeniranje: 2.5<br>
//    3. ISO 14001. Pondeniranje: 2.5<br>


        return tender;
    }

}
