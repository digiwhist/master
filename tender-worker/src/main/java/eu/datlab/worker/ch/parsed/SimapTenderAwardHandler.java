package eu.datlab.worker.ch.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import static eu.datlab.worker.ch.parsed.SimapTenderParser.parseAnythingRightUnder;
import static eu.datlab.worker.ch.parsed.SimapTenderParser.selectTextUnderHeader;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by michalriha on 20/03/2017.
 */
public final class SimapTenderAwardHandler {

    /**
     * Private constructor.
     */
    private SimapTenderAwardHandler() {
    }

    /**
     * Main method to parse award forms.
     *
     * @param parsedTender parsed tender to fill
     * @param document     document to parse from
     *
     * @return ParsedTender
     */
    public static ParsedTender parse(final ParsedTender parsedTender, final Document document) {
        return parsedTender
                .addPublications(parseAdditionalPublications(document))
                .setAddressOfImplementation(new ParsedAddress()
                        .setRawAddress(selectTextUnderHeader("2.6", document)))
                .setIsOnBehalfOf(
                        parseAnythingRightUnder("span:containsOwn(Service demandeur/Entité adjudicatrice)", document))
                .addBuyer(new ParsedBody()
                        .setName(parseAnythingRightUnder(new String[]{
                                "span:containsOwn(Service organisateur/Entité organisatrice)",
                                "span:containsOwn(Bedarfsstelle/Vergabestelle)"}, document))
                        .setAddress(new ParsedAddress()
                                .setRawAddress(parseAnythingRightUnder(new String[]{
                                        "span:containsOwn(Service organisateur/Entité organisatrice)",
                                        "span:containsOwn(Beschaffungsstelle/Organisator)"}, document)))
                        .setBuyerType(selectTextUnderHeader("1.2", document)))
                .setNationalProcedureType(selectTextUnderHeader("1.3", document))
                .setSupplyType(selectTextUnderHeader("1.4", document))
                .setIsCoveredByGpa(selectTextUnderHeader("1.5", document))
                .setTitle(selectTextUnderHeader("2.1", document))
                .setCpvs(parseCPVs(document))
                .setAwardDecisionDate(selectTextUnderHeader("4.2", document))
                .addLot(parseLot(document));
    }

    /**
     * Parse lot.
     *
     * @param document document to parse from
     *
     * @return ParsedTenderLot
     */
    private static ParsedTenderLot parseLot(final Document document) {
        List<ParsedBody> bidders = new ArrayList<>();
        String name = parseAnythingRightUnder(
                "h4:containsOwn(Liste des adjudicataires) + dl span:containsOwn" +
                        "(Nom)", document);
        String address = parseAnythingRightUnder(
                "h4:containsOwn(Liste des adjudicataires) + dl " +
                        "span:containsOwn(Nom)", document);
        String price = parseAnythingRightUnder("h4:containsOwn(Liste des adjudicataires) + dl " +
                "span:containsOwn(Prix)", document);
        String vat = null;
        ParsedPrice parsedPrice = new ParsedPrice().setNetAmount(price);
        if(name != null && address != null){
            bidders.add(new ParsedBody()
                    .setName(name)
                    .setAddress(new ParsedAddress().setRawAddress(address)));
        } else {
            Element biddersElem = JsoupUtils.selectFirst("h4:containsOwn(Liste der Anbieter)", document);
            if(biddersElem == null){
                return null;
            }
            biddersElem = biddersElem.parent().selectFirst("dl");
            for(Element bidderElem: biddersElem.select("dd")){
                String info = bidderElem.text();
                String nameDelim = "Name: ", priceDelim = "Preis:";
                if(!info.contains(nameDelim)){
                    nameDelim = "";
                }
                if(!info.contains(priceDelim)){
                    priceDelim = "Preisspanne der eingegangenen Angebote:";
                }
                if(nameDelim.isEmpty()){
                    name = info.split("Preis:")[0].split(",")[0];
                } else {
                    name = info.split("Preis:")[0].split(nameDelim)[1].split(",")[0];
                }
                address = info.split(priceDelim)[0].split(nameDelim)[1].replace(name + ",", "");
                if(info.contains(priceDelim)) {
                    price = info.split(priceDelim)[1].split("Bemerkung")[0];
                    if(priceDelim.equals("Preis:")){
                        for(String part: price.split(" ")){
                            if(part.isEmpty()){
                                continue;
                            } else if(!part.isEmpty() && part.equals(part.toUpperCase()) && parsedPrice.getCurrency() == null){
                                parsedPrice.setCurrency(part);
                            } else if(Character.isDigit(part.charAt(0))){
                                parsedPrice.setNetAmount(part);
                            } else if(part.equals("ohne")){
                                vat = "false";
                            } else if(part.contains("MWSt") && vat == null){
                                vat = "true";
                            }
                        }
                        parsedPrice.setVat(vat);
                    } else {
                        // parse interval
                        boolean from = false;
                        for(String part: price.split(" ")){
                            if(part.isEmpty()){
                                continue;
                            } else if(part.equals(part.toUpperCase()) && parsedPrice.getCurrency() == null){
                                parsedPrice.setCurrency(part);
                            } else if(Character.isDigit(part.charAt(0))){
                                if(!from){
                                    parsedPrice.setMinNetAmount(part);
                                } else {
                                    parsedPrice.setMaxNetAmount(part);
                                }
                            } else if(part.equals("von")){
                                from = false;
                            } else if(part.contains("bis")){
                                from = true;
                            }
                        }
                    }

                }
                bidders.add(new ParsedBody().setName(name).setAddress(new ParsedAddress().setRawAddress(address)));
            }

        }

        if(bidders.isEmpty()){
            bidders = null;
        }
        return new ParsedTenderLot()
                .setBidsCount("1")
                .addBid(new ParsedBid()
                        .setIsWinning(String.valueOf(true))
                        .setBidders(bidders)
                        .setPrice(parsedPrice));
    }


    /**
     * Parse additional publications.
     *
     * @param document document to parse from
     *
     * @return List<ParsedPublication> or null
     */
    private static List<ParsedPublication> parseAdditionalPublications(final Document document) {
        Elements publicationIds = document.select("h1:containsOwn(Appel d\\'offres) + dl > dd:containsOwn(VD)");

        if (publicationIds == null || publicationIds.isEmpty()) {
            publicationIds = document.select("h1:containsOwn(Appel d\\'offres) + dl > dd:containsOwn(GR)");
        }

        if (publicationIds == null || publicationIds.isEmpty()) {
            return null;
        }

        final List<ParsedPublication> parsedPublications = new ArrayList<>();

        for (Element publicationId : publicationIds) {
            parsedPublications.add(new ParsedPublication()
                    .setSourceId(publicationId.text())
                    .setIsIncluded(false)
                    .setSource(PublicationSources.CH_SIMAP));
        }

        return parsedPublications;
    }

    /**
     * Parse cpvs.
     *
     * @param document document to parse from
     *
     * @return List<ParsedCPV> or null
     */
    private static List<ParsedCPV> parseCPVs(final Document document) {
        final Elements cpvs = document.select("td:contains(CPV:) + td");

        if (cpvs == null || cpvs.isEmpty()) {
            return null;
        }

        final List<ParsedCPV> parsedCpvs = new ArrayList<>();

        for (Element cpv : cpvs) {
            String code = cpv.text();
            if(code != null && !code.isEmpty()){
                for(String part: code.split(" ")){
                    if(!part.isEmpty() && part.matches("[0-9]*")){
                        code = part;
                    }
                }
                parsedCpvs.add(new ParsedCPV()
                        .setCode(code)
                        .setIsMain(String.valueOf(parsedCpvs.isEmpty())));
            }
        }

        return parsedCpvs;
    }
}
