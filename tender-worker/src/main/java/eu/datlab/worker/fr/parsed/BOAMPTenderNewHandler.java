package eu.datlab.worker.fr.parsed;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.fr.BOAMPTenderUtils;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedBid;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedCorrigendum;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;
import eu.dl.dataaccess.dto.raw.RawData;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.parser.Parser;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * New form parser for France.
 *
 * @author Marek Mikes
 */
final class BOAMPTenderNewHandler {
    private static final Logger logger = LoggerFactory.getLogger(BOAMPTenderNewHandler.class);

    /**
     * Private constructor to make this class static.
     */
    private BOAMPTenderNewHandler() {
    }

    /**
     * Parses data for all old forms (contract notice and contract award).
     *
     * @param rawTender raw tender to be parsed
     * @return List<ParsedTender> with parsed data
     */
    static List<ParsedTender> parse(final RawData rawTender) {
        ParsedTender parsedTender = new ParsedTender();

        final Document document = Jsoup.parse(rawTender.getSourceData(), "", Parser.xmlParser());
        final Boolean isFrameworkAgreement = JsoupUtils.exists("DONNEES > PROCEDURE > AVIS_IMPLIQUE > ACCORD_CADRE_OUI",
                document);
        parsedTender
                .setPublications(parsePublications(document, rawTender.getSourceUrl().toString(),
                        rawTender.getSourceFileName()))
                .addBuyer(new ParsedBody()
                        .setName(JsoupUtils.selectText("DONNEES > IDENTITE > DENOMINATION", document))
                        .setContactPoint(JsoupUtils.selectText("DONNEES > IDENTITE > CONTACT", document))
                        .setAddress(new ParsedAddress()
                                .setPostcode(JsoupUtils.selectText("DONNEES > IDENTITE > CP", document))
                                .setCity(JsoupUtils.selectText("DONNEES > IDENTITE > VILLE", document))
                                .setCountry(JsoupUtils.selectText("DONNEES > IDENTITE > PAYS", document))
                                .addNuts(JsoupUtils.selectText("DONNEES > IDENTITE > ADJUDICATEUR_NUTS", document))
                                .setStreet(JsoupUtils.selectText("DONNEES > IDENTITE > ADRESSE", document))
                                .setUrl(JsoupUtils.selectText("DONNEES > IDENTITE > URL", document)))
                        .setPhone(JsoupUtils.selectText("DONNEES > IDENTITE > TEL", document))
                        .addMainActivity(JsoupUtils.selectNodeName(
                                "DONNEES > ACTIVITE_PRINCIPALE > POUVOIR_ADJUDICATEUR > *, " +
                                        "DONNEES > ACTIVITE_PRINCIPALE > ENTITE_ADJUDICATRICE > *", document))
                        .setBuyerType(JsoupUtils.selectNodeName("DONNEES > TYPE_ORGANISME > *, " +
                                "DONNEES > TYPE_POUVOIR_ADJUDICATEUR > *", document)))
                .setIsCentralProcurement(JsoupUtils.exists(
                        "DONNEES > IDENTITE > ORGANISME_ACHETEUR_CENTRAL_OUI", document).toString())
                .setTitle(JsoupUtils.selectText("DONNEES > OBJET > TITRE_MARCHE", document))
                .setAddressOfImplementation(new ParsedAddress()
                        .setPostcode(JsoupUtils.selectText("DONNEES > OBJET > LIEU_EXEC_LIVR > CP", document))
                        .setCity(JsoupUtils.selectText("DONNEES > OBJET > LIEU_EXEC_LIVR > VILLE", document))
                        .setRawAddress(JsoupUtils.selectText("DONNEES > OBJET > LIEU_EXEC_LIVR > ADRESSE", document))
                        .addNuts(JsoupUtils.selectText("DONNEES > OBJET > LIEU_EXEC_LIVR > CODE_NUTS", document)))
                .setCpvs(parseCpvs(document))
                .setSupplyType(JsoupUtils.selectNodeName("DONNEES > OBJET > TYPE_MARCHE > *", document))
                .setEstimatedDurationInYears(JsoupUtils.selectText("DONNEES > OBJET > DELEGATION > DUREE_AN", document))
                .setEstimatedDurationInMonths(JsoupUtils.selectText("DONNEES > OBJET > DELEGATION > DUREE_MOIS",
                        document))
                .setEstimatedStartDate(StringUtils.removeDotsAtTheEnd(JsoupUtils.selectText(
                        "DONNEES > OBJET > DATE_LANCEMENT", document)))
                .addCorrigendum(parseEstimatedStartDateCorrigendum(document))
                .setHasLots(JsoupUtils.exists("DONNEES > OBJET > DIV_EN_LOTS > OUI", document).toString())
                .setLots(parseTenderLots(document, isFrameworkAgreement))
                .setProcedureType(JsoupUtils.selectNodeName("DONNEES > PROCEDURE > TYPE_PROCEDURE > *", document))
                .setIsAcceleratedProcedure(JsoupUtils.exists("DONNEES > PROCEDURE > RESTREINT > ACCELERE", document)
                        .toString())
                .setEnvisagedMaxCandidatesCount(JsoupUtils.selectText(
                        "DONNEES > PROCEDURE > NB_CANDIDATS > NB_MAX_OFFRE", document))
                .setEnvisagedMinCandidatesCount(JsoupUtils.selectText(
                        "DONNEES > PROCEDURE > NB_CANDIDATS > NB_MIN_OFFRE", document))
                .setIsFrameworkAgreement(isFrameworkAgreement.toString())
                .setIsDps(JsoupUtils.exists("DONNEES > PROCEDURE > AVIS_IMPLIQUE > SAD_OUI", document).toString())
                .setMaxFrameworkAgreementParticipants(JsoupUtils.selectText(
                        "DONNEES > PROCEDURE > ACCORD_CADRE > NB_MAX_PARTICIPANTS", document))
                .setAwardCriteria(parseTenderAwardCriteria(document))
                .setEstimatedPrice(parseEstimatedPrice(document))
                .setAwardDecisionDate(JsoupUtils.selectText("DONNEES > ATTRIBUTION > DATE_DECISION", document))
                .setCorrections(parseCorrections(document))
                .setIsWholeTenderCancelled(JsoupUtils.exists("DONNEES > ANNULATION", document).toString())
                .setCancellationReason(JsoupUtils.selectText("DONNEES > ANNULATION > MOTIF", document))
                .setEligibilityCriteria(JsoupUtils.selectText("DONNEES > CONDITION_PARTICIPATION > CRITERE_SELECTION",
                        document))
                .setPersonalRequirements(JsoupUtils.selectText(
                        "DONNEES > CONDITION_PARTICIPATION > SITUATION_JURIDIQUE", document))
                .setEconomicRequirements(JsoupUtils.selectText("DONNEES > CONDITION_PARTICIPATION > CAP_ECO",
                        document))
                .setTechnicalRequirements(JsoupUtils.selectText("DONNEES > CONDITION_PARTICIPATION > CAP_TECH",
                        document))
                .setIsDocumentsAccessRestricted(JsoupUtils.exists(
                        "DONNEES > CONDITION_ADMINISTRATIVE > CONDITIONS_ET_MODE_PAIEMENT_OBTENIR_DOCUMENTS", document)
                        .toString())
                .setDocumentsDeadline(JsoupUtils.selectText(
                        "DONNEES > CONDITION_ADMINISTRATIVE > DATE_LIMITE_OBTENTION_DOCUMENTS", document))
                .setDocumentsPayable(JsoupUtils.exists("DONNEES > CONDITION_ADMINISTRATIVE > DOCUMENT_PAYANT_OUI",
                        document).toString())
                .setDocumentsPrice(parseDocumentsPrice(document))
                .setDescription(parseTenderDescription(document));

        return Arrays.asList(parsedTender);
    }

    /**
     * Parses info about this publication and all the related ones.
     *
     * @param document           document to be parsed
     * @param machineReadableUrl machine readable URL to be set
     * @param sourceFileName     source file name
     * @return list of publications
     */
    private static List<ParsedPublication> parsePublications(final Document document,
                                                             final String machineReadableUrl,
                                                             final String sourceFileName) {
        List<ParsedPublication> publications = new ArrayList<>();

        final String sourceId = BOAMPTenderUtils.getPublicationSourceIdFrom(sourceFileName);

        // the publication in XML file
        publications.add(new ParsedPublication()
                .setIsIncluded(true)
                .setSourceFormType(JsoupUtils.selectNodeName("GESTION > REFERENCE > TYPE_AVIS > NATURE > *", document))
                .setPublicationDate(JsoupUtils.selectText("GESTION > INDEXATION > DATE_PUBLICATION", document))
                .setMachineReadableUrl(machineReadableUrl)
                .setHumanReadableUrl(String.format(BOAMPTenderUtils.PUBLICATION_PERMALINK_PATTERN, sourceId))
                .setSource(PublicationSources.FR_BOAMP_FTP)
                .setSourceId(sourceId));

        // related publications in XML file
        Elements relatedPublicationElements = JsoupUtils.select("GESTION > MARCHE > ANNONCE_ANTERIEUR", document);
        if (relatedPublicationElements != null) {
            for (Element relatedPublicationElement : relatedPublicationElements) {
                publications.add(new ParsedPublication()
                        .setIsIncluded(false)
                        .setDispatchDate(StringUtils.removeDotsAtTheEnd(JsoupUtils.selectText(
                                "REFERENCE > DATE_ENVOI", relatedPublicationElement)))
                        .setPublicationDate(JsoupUtils.selectText(
                                "REFERENCE_PUBLICATION > PUBLICATION_PAPIER > DATE_PUBLICATION",
                                relatedPublicationElement))
                        .setSource(PublicationSources.FR_BOAMP_FTP)
                        .setSourceId(JsoupUtils.selectText("REFERENCE > IDWEB", relatedPublicationElement)));
            }
        }

        return publications;
    }

    /**
     * Parse CPVs from element.
     *
     * @param element element to be parsed
     * @return list of parsed CPVs or Null
     */
    private static List<ParsedCPV> parseCpvs(final Element element) {
        List<ParsedCPV> tenderCpvs = new ArrayList<>();

        String mainCpvCode = JsoupUtils.selectText("DONNEES > OBJET > CPV > PRINCIPAL", element);
        if (mainCpvCode != null) {
            tenderCpvs.add(new ParsedCPV()
                    .setCode(mainCpvCode)
                    .setIsMain(Boolean.TRUE.toString()));
        }

        Elements complementCpvCodeElements = JsoupUtils.select("DONNEES > OBJET > CPV > SUPPLEMENTAIRE", element);
        if (complementCpvCodeElements != null) {
            for (Element complementCpvCodeElement : complementCpvCodeElements) {
                tenderCpvs.add(new ParsedCPV()
                        .setCode(complementCpvCodeElement.text())
                        .setIsMain(Boolean.FALSE.toString()));
            }
        }

        return tenderCpvs.isEmpty() ? null : tenderCpvs;
    }

    /**
     * Parses all the lots from document.
     *
     * @param document document to be parsed
     * @param isFrameworkAgreement says whether the tender is framework agreement
     * @return list of all parsed lots or empty list if no lots specified
     */
    private static List<ParsedTenderLot> parseTenderLots(final Document document, final Boolean isFrameworkAgreement) {
        List<ParsedTenderLot> lots1 = new ArrayList<>();
        Elements lotElements = JsoupUtils.select("DONNEES > OBJET > LOTS", document);
        for (Element lotElement : lotElements) {
            lots1.add(new ParsedTenderLot()
                    .setLotNumber(JsoupUtils.selectText("NUM", lotElement))
                    .setTitle(JsoupUtils.selectText("INTITULE", lotElement))
                    .setDescription(JsoupUtils.selectText("DESCRIPTION", lotElement))
                    .setCpvs(parseCpvs(lotElement))
                    .setAddressOfImplementation(new ParsedAddress()
                            .addNuts(JsoupUtils.selectText("CODE_NUTS", lotElement))
                            .setRawAddress(JsoupUtils.selectText("LIEU_PRINCIPAL", lotElement)))
                    .addAwardCriterion(new ParsedAwardCriterion()
                            .setDescription(JsoupUtils.selectText("CRITERES_ATTRIBUTION", lotElement)))
                    .setEnvisagedMinCandidatesCount(JsoupUtils.selectText("NB_CANDIDATS > NB_MIN_OFFRE", lotElement))
                    .setEnvisagedMaxCandidatesCount(JsoupUtils.selectText("NB_CANDIDATS > NB_MAX_OFFRE", lotElement))
                    .setEnvisagedCandidatesCount(JsoupUtils.selectText("NB_CANDIDATS > NB_OFFRE", lotElement))
                    .setLimitedCandidatesCountCriteria(JsoupUtils.selectText("NB_CANDIDATS > LIMITATION_CANDIDATS",
                            lotElement))
                    .setAreVariantsAccepted(JsoupUtils.exists("VARIANTES_OUI", lotElement).toString()));
        }

        List<ParsedTenderLot> lots2 = new ArrayList<>();
        lotElements = JsoupUtils.select("DONNEES > ATTRIBUTION > DECISION", document);
        for (Element lotElement : lotElements) {
            ParsedTenderLot lot = new ParsedTenderLot()
                    .setContractNumber(JsoupUtils.selectText("NUM_MARCHE", lotElement))
                    .setLotNumber(JsoupUtils.selectText("NUM_LOT", lotElement))
                    .setBidsCount(JsoupUtils.selectText("RENSEIGNEMENT > NB_OFFRE_RECU", lotElement))
                    .setSmeBidsCount(JsoupUtils.selectText("RENSEIGNEMENT > NB_OFFRE_RECU_PME", lotElement))
                    .setOtherEuMemberStatesCompaniesBidsCount(JsoupUtils.selectText(
                            "RENSEIGNEMENT > NB_OFFRE_RECU_UE", lotElement))
                    .setNonEuMemberStatesCompaniesBidsCount(JsoupUtils.selectText(
                            "RENSEIGNEMENT > NB_OFFRE_RECU_NON_UE", lotElement))
                    .setValidBidsCount(JsoupUtils.selectText("RENSEIGNEMENT > NB_OFFRE_RECU_ELECT", lotElement));

            List<ParsedBody> bidders = new ArrayList<>();
            Elements bidderElements = JsoupUtils.select("TITULAIRE", lotElement);
            for (Element bidderElement : bidderElements) {
                bidders.add(new ParsedBody()
                        .setName(JsoupUtils.selectText("DENOMINATION", bidderElement))
                        .addBodyId(new BodyIdentifier()
                                .setId(JsoupUtils.selectText("CODE_IDENT_NATIONAL", bidderElement))
                                .setType(BodyIdentifier.Type.ORGANIZATION_ID))
                        .setContactName(JsoupUtils.selectText("CORRESPONDANT", bidderElement))
                        .setAddress(new ParsedAddress()
                                .setStreet(JsoupUtils.selectText("ADRESSE", bidderElement))
                                .setPostcode(JsoupUtils.selectText("CP", bidderElement))
                                .setCity(JsoupUtils.selectText("VILLE", bidderElement))
                                .addNuts(JsoupUtils.selectText("CODE_NUTS", bidderElement))
                                .setCountry(JsoupUtils.selectText("PAYS", bidderElement))
                                .setUrl(JsoupUtils.selectText("URL", bidderElement)))
                        .setPhone(JsoupUtils.selectText("TEL", bidderElement))
                        .setIsSme(JsoupUtils.exists("PME_OUI", bidderElement).toString()));
            }

            Element lotFinalPriceElement = JsoupUtils.selectFirst("RENSEIGNEMENT > MONTANT", lotElement);
            ParsedPrice lotFinalPrice = lotFinalPriceElement == null
                    ? null
                    : new ParsedPrice()
                    .setNetAmount(lotFinalPriceElement.text())
                    .setCurrency(JsoupUtils.selectAttribute("DEVISE", lotFinalPriceElement));

            final Boolean isConsortium = JsoupUtils.exists("RENSEIGNEMENT > GROUPEMENT_ECONOMIQUE", lotElement);
            if (bidders.size() > 1 && !isConsortium && !isFrameworkAgreement) {
                logger.warn("Bid has more than one bidder and it is not a consortium nor framework agreement. " +
                        "It could not happen!!!");
            }
            lot.addBid(new ParsedBid()
                    .setIsWinning(Boolean.TRUE.toString())
                    .setBidders(bidders)
                    .setIsConsortium(isConsortium.toString())
                    .setPrice(lotFinalPrice));

            lots2.add(lot);
        }

        if (lots1.isEmpty() && lots2.isEmpty()) {
            return null;
        } else if (!lots1.isEmpty() && lots2.isEmpty()) {
            return lots1;
        } else if (lots1.isEmpty() && !lots2.isEmpty()) {
            return lots2;
        } else {
            // join the lists through lot number and return result
            while (!lots2.isEmpty()) {
                ParsedTenderLot lotToJoin = lots2.remove(0);
                if (lotToJoin.getLotNumber() != null) {
                    List<ParsedTenderLot> lotsWhereToJoin = lots1
                            .stream()
                            .filter(l -> l.getLotNumber() != null)
                            .filter(l -> l.getLotNumber().equals(lotToJoin.getLotNumber()))
                            .collect(Collectors.toList());
                    if (!lotsWhereToJoin.isEmpty()) {
                        assert lotsWhereToJoin.size() == 1;
                        ParsedTenderLot lotWhereToJoin = lotsWhereToJoin.get(0);
                        // set all from lots2
                        lotWhereToJoin
                                .setContractNumber(lotToJoin.getContractNumber())
                                .setBidsCount(lotToJoin.getBidsCount())
                                .setSmeBidsCount(lotToJoin.getSmeBidsCount())
                                .setOtherEuMemberStatesCompaniesBidsCount(
                                        lotToJoin.getOtherEuMemberStatesCompaniesBidsCount())
                                .setNonEuMemberStatesCompaniesBidsCount(
                                        lotToJoin.getNonEuMemberStatesCompaniesBidsCount())
                                .setValidBidsCount(lotToJoin.getValidBidsCount())
                                .setBids(lotToJoin.getBids());
                    } else {
                        // we want to join lot which is not in the result yet -> add it to the result
                        lots1.add(lotToJoin);
                    }
                } else {
                    // we want to join lot which does not have lot number -> add it to the result
                    lots1.add(lotToJoin);
                }
            }
            return lots1;
        }
    }

    /**
     * Parse tender award criterion list from publication element.
     *
     * @param document document to be parsed
     * @return award criterion list or Null
     */
    private static List<ParsedAwardCriterion> parseTenderAwardCriteria(final Document document) {
        Elements awardCriterionElements = JsoupUtils.select(
                "DONNEES > PROCEDURE > CRITERES_ATTRIBUTION > CRITERES_PONDERES > CRITERE", document);
        if (awardCriterionElements.isEmpty()) {
            return null;
        }

        List<ParsedAwardCriterion> awardCriteria = new ArrayList<>();

        for (Element awardCriterionElement : awardCriterionElements) {
            awardCriteria.add(new ParsedAwardCriterion()
                    .setWeight(JsoupUtils.selectAttribute("POIDS", awardCriterionElement))
                    .setName(awardCriterionElement.text()));
        }

        return awardCriteria;
    }

    /**
     * Parses tender estimated price.
     *
     * @param document document to be parsed
     * @return tender estimated price
     */
    private static ParsedPrice parseEstimatedPrice(final Document document) {
        final Element estimatedPriceElement = JsoupUtils.selectFirst("DONNEES > ATTRIBUTION > VALEUR_TOTALE", document);
        if (estimatedPriceElement == null) {
            return null;
        }

        return new ParsedPrice()
                .setNetAmount(estimatedPriceElement.text())
                .setCurrency(JsoupUtils.selectAttribute("DEVISE", estimatedPriceElement));
    }

    /**
     * Parses all the corrections.
     *
     * @param document document to be parsed
     * @return list of corrections
     */
    static List<ParsedCorrigendum> parseCorrections(final Document document) {
        Elements correctionElements = JsoupUtils.select("DONNEES > RECTIF > MODIFICATION", document);
        if (correctionElements.isEmpty()) {
            return null;
        }

        final List<ParsedCorrigendum> corrections = new ArrayList<>();

        for (Element correctionElement : correctionElements) {
            ParsedCorrigendum corrigendum = new ParsedCorrigendum()
                    .setLotNumber(JsoupUtils.selectAttribute("LOT", correctionElement))
                    .setOriginal(JsoupUtils.selectAttribute("TEXTE > INIT", correctionElement))
                    .setReplacement(JsoupUtils.selectAttribute("TEXTE > LIRE", correctionElement))
                    .setOriginalDate(JsoupUtils.selectAttribute("DATE > INIT", correctionElement))
                    .setReplacementDate(JsoupUtils.selectAttribute("DATE > LIRE", correctionElement));

            // CPVs
            String originalMainCpvCode = JsoupUtils.selectAttribute("CPV > INIT > PRINCIPAL", correctionElement);
            if (originalMainCpvCode != null) {
                corrigendum.addOriginalCpv(new ParsedCPV()
                        .setCode(originalMainCpvCode)
                        .setIsMain(Boolean.TRUE.toString()));
            }
            Elements originalNonMainCpvCodes = JsoupUtils.select("CPV > INIT > SUPPLEMENTAIRE", document);
            for (Element originalNonMainCpvCode : originalNonMainCpvCodes) {
                corrigendum.addOriginalCpv(new ParsedCPV()
                        .setCode(originalNonMainCpvCode.text())
                        .setIsMain(Boolean.FALSE.toString()));
            }
            String replacementMainCpvCode = JsoupUtils.selectAttribute("CPV > LIRE > PRINCIPAL", correctionElement);
            if (replacementMainCpvCode != null) {
                corrigendum.addOriginalCpv(new ParsedCPV()
                        .setCode(replacementMainCpvCode)
                        .setIsMain(Boolean.TRUE.toString()));
            }
            Elements replacementNonMainCpvCodes = JsoupUtils.select("CPV > LIRE > SUPPLEMENTAIRE", document);
            for (Element replacementNonMainCpvCode : replacementNonMainCpvCodes) {
                corrigendum.addOriginalCpv(new ParsedCPV()
                        .setCode(replacementNonMainCpvCode.text())
                        .setIsMain(Boolean.FALSE.toString()));
            }

            corrections.add(corrigendum);

        }

        return corrections;
    }

    /**
     * Parses documents price.
     *
     * @param document document to be parsed
     * @return documents price
     */
    private static ParsedPrice parseDocumentsPrice(final Document document) {
        Element priceElement = JsoupUtils.selectFirst(
                "DONNEES > CONDITION_ADMINISTRATIVE > DOCUMENT_PAYANT_OUI > VALEUR", document);
        if (priceElement == null) {
            return null;
        }

        return new ParsedPrice()
                .setNetAmount(priceElement.text())
                .setCurrency(JsoupUtils.selectAttribute("DEVISE", priceElement));
    }

    /**
     * Parses estimated start date corrigendum.
     *
     * @param document document to be parsed
     * @return corrigendum or null
     */
    private static ParsedCorrigendum parseEstimatedStartDateCorrigendum(final Document document) {
        Element estimatedStartDateCorrigendumElement = JsoupUtils.selectFirst(
                "DONNEES > OBJET > DELEGATION > DATE_DEBUT", document);
        if (estimatedStartDateCorrigendumElement == null) {
            return null;
        }

        return new ParsedCorrigendum()
                .setReplacementDate(estimatedStartDateCorrigendumElement.text());
    }

    /**
     * Parses tender description.
     *
     * @param document document to be parsed
     * @return tender description or null
     */
    static String parseTenderDescription(final Document document) {
        final String descriptionCode = JsoupUtils.selectText(
                "GESTION > INDEXATION > DESCRIPTEURS > DESCRIPTEUR > CODE", document);
        final String descriptionLabel = JsoupUtils.selectText(
                "GESTION > INDEXATION > DESCRIPTEURS > DESCRIPTEUR > LIBELLE", document);
        if (descriptionCode != null && descriptionLabel != null) {
            return descriptionCode + " ; " + descriptionLabel;
        } else if (descriptionCode != null) {
            return descriptionCode;
        } else if (descriptionLabel != null) {
            return descriptionLabel;
        } else {
            return null;
        }
    }

}
