package eu.datlab.worker.fr.parsed;

import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedAwardCriterion;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.worker.utils.StringUtils;
import eu.dl.worker.utils.jsoup.JsoupUtils;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Old contract notice form parser for France.
 *
 * @author Marek Mikes
 */
final class BOAMPTenderOldContractNoticeHandler {
    private static final Logger logger = LoggerFactory.getLogger(BOAMPTenderOldContractNoticeHandler.class);

    /**
     * Private constructor to make this class static.
     */
    private BOAMPTenderOldContractNoticeHandler() {}

    /**
     * Parses data for old contract notice forms.
     *
     * @param publicationElement
     *         element to parse data from
     * @param parsedTender
     *         tender to add data to
     */
    static void parse(final Element publicationElement, final ParsedTender parsedTender) {
        parsedTender
                .setDescription(JsoupUtils.selectText("DONNEES > CARACTERISTIQUES > QUANTITE", publicationElement))
                .setEstimatedStartDate(StringUtils.removeDotsAtTheEnd(JsoupUtils.selectText(
                        "DONNEES > DUREE > DEB_PRESTATION", publicationElement)))
                .setEligibleBidLanguages(parseTenderEligibleBidLanguages(publicationElement))
                .setAwardCriteria(parseTenderAwardCriteria(publicationElement))
                .setProcedureType(JsoupUtils.selectAttribute("DONNEES > PROCEDURE", "type", publicationElement))
                .setBidDeadline(parseTenderBidDeadline(publicationElement))
                .setDocumentsLocation(parseTenderDocumentsLocation(publicationElement))
                .setAwardDeadline(JsoupUtils.selectText("DONNEES > DELAI > VALIDITE_OFFRE", publicationElement));

        parsedTender.getBuyers().get(0)
                .setContactPoint(JsoupUtils.selectText("DONNEES > CORRESPONDANTS > ADMIN_TECH", publicationElement));

        parsedTender.getPublications().get(0)
                .setDispatchDate(StringUtils.removeDotsAtTheEnd(JsoupUtils.selectText("DONNEES > ENVOI_BOMP",
                        publicationElement)));
    }

    /**
     * Parse tender eligible bid languages from publication element.
     *
     * @param publicationElement
     *         publication element to be parsed
     *
     * @return list of languages or Null
     */
    private static List<String> parseTenderEligibleBidLanguages(final Element publicationElement) {
        String eligibleBidLanguage = JsoupUtils.selectText("DONNEES > CONDITIONS > RELATIVES > LANGUE",
                publicationElement);
        if (eligibleBidLanguage == null) {
            return null;
        }
        return Arrays.asList(StringUtils.removeDotsAtTheEnd(eligibleBidLanguage));
    }

    /**
     * Parse tender award conditions list from publication element.
     *
     * @param publicationElement
     *         publication element to be parsed
     *
     * @return award criterion list or empty list
     */
    private static List<ParsedAwardCriterion> parseTenderConditions(final Element publicationElement) {
        Element criteriaNode = JsoupUtils.selectFirst("DONNEES > CONDITIONS > CRITERES > TITRE:contains(Critères d\\'attribution)",
            publicationElement);

        List<ParsedAwardCriterion> awardCriteria = new ArrayList<>();

        if (criteriaNode != null) {
            Element n = criteriaNode;
            while ((n = n.nextElementSibling()) != null) {
                awardCriteria.add(new ParsedAwardCriterion().setName(n.nodeName()));
            }
        }
        
        return awardCriteria;
    }

    /**
     * Parse tender award criterion list from publication element.
     *
     * @param publicationElement
     *         publication element to be parsed
     *
     * @return award criterion list or Null
     */
    private static List<ParsedAwardCriterion> parseTenderAwardCriteria(final Element publicationElement) {
        List<ParsedAwardCriterion> awardCriteria = new ArrayList<>();

        awardCriteria.addAll(parseTenderConditions(publicationElement));

        String awardCriteriaString = JsoupUtils.selectText("DONNEES > CONDITIONS > CRITERES > LISTE",
            publicationElement);
        if (awardCriteriaString == null) {
            return awardCriteria.isEmpty() ? null : awardCriteria;
        }

        // Sometimes award criteria are not filled in percents: "- 1) valeur technique (manoeuvrabilit�, compacit� du
        // mat�riel, capacit� de levage, garantie, maintenance) note sur 10 points (croissante au m�rite),
        // coefficient 5 ; - 2) prix (machine neuve + forfait de maintenance) note sur 10 points, coefficient 3 ; - 3)
        // d�lai de livraison (chariot t�lescopique + pi�ces d�tach�es) note sur 10 points, coefficient 2."
        // I do not know how to parse, so I set it to description
        if (!awardCriteriaString.contains("%")) {
            awardCriteria.add(new ParsedAwardCriterion().setDescription(awardCriteriaString));
            return awardCriteria;
        }

        // Usually colon is before percent (delimiter is ':'), but not always (delimiter is '('). See
        //   "- prix (40 %) ; - disponibilit�s des produits demand�s (40 %) ; - d�lais de livraison (20 %)."
        // And usually between number and percent sign is space, but not always. See
        //   "- prix des prestations : 60% valeur technique des prestations : 40%."
        // When colon is delimiter, space is before the colon. We use the space in regular expression, otherwise we
        // choose incorrectly the colon as delimiter for incorrect award criteria:
        //   "- valeur technique de l'offre sera not�e de 1 � 5, affect�e d'un coefficient de 7 soit 70 % de
        //   pond�ration (5 correspondant � la meilleure note). le calacul de la note se fera par attribution de points,
        //   totalisant un maximum de 5 points. la valeur technique sera appr�ci�e de la fa�on suivante: 1- les d�lais
        //   propos�s pour la remise des documents=2,5 points. 2- le temps pr�visionnel d'intervention pour les phases
        //   conception et r�alisation=0,5 point. 3- le nombre de participations pr�vues aux r�unions de
        //   chantier=1 point. 4- le nombre de visites compl�mentaires pr�vues sur le chantier=1 point. ; - prix des
        //   prestations sera not� de 1 � 5, affect� d'un coefficient de 3 soit 30 % de pond�ration (5 �tant la
        //   meilleure note). le crit�re analys� est le prix global et forfaitaire de l'offre."
        // In pattern below is not '%', because some award criterion looks something like " : 10 points %"
        Matcher m = Pattern.compile(" : \\d{1,3}").matcher(awardCriteriaString);
        boolean isColonBeforePercent = m.find();
        char nameWeightDelimiter;
        if (isColonBeforePercent) {
            nameWeightDelimiter = ':';
        } else {
            m = Pattern.compile(" \\(\\d{1,3} %").matcher(awardCriteriaString);
            boolean isPercentInBrackets = m.find();
            if (isPercentInBrackets) {
                nameWeightDelimiter = '(';
            } else {
                // award criteria are probably filled incorrectly. E.g. "- lot1 prix 80% etendue de gamme 20% ; - lot 2
                // qualit� �chantillons 40% etendue de gamme 10% prix 50%."
                awardCriteria.add(new ParsedAwardCriterion().setDescription(awardCriteriaString));
                return awardCriteria;
            }
        }

        List<String> awardCriterionStrings = new ArrayList<>(Arrays.asList(awardCriteriaString.split(" ; -")));
        // Splitting by the " ; -" is not always correct. See "- valeur technique de l'offre (- pertinence de la
        // m�thodologie propos�e et ad�quation avec les objectifs fix�s 20 % ; - qualit� des r�f�rences de l'�quipe
        // projet30 %) : 50 % ; - prix : 30 % ; - d�lai d'ex�cution : 20 %."
        // In that case we join the parts back
        boolean twoAwardCriteriaWasMerged;
        do {
            twoAwardCriteriaWasMerged = false;
            for (int i = 0; i < awardCriterionStrings.size() - 1; ++i) {
                if (awardCriterionStrings.get(i).lastIndexOf(nameWeightDelimiter) == -1
                        || awardCriterionStrings.get(i).lastIndexOf('%') == -1) {
                    logger.debug("We are joining \"{}\" and \"{}\", because they represents one award criterion",
                        awardCriterionStrings.get(i), awardCriterionStrings.get(i + 1));
                    // merge this string with next string, because they are one award criterion
                    assert i < awardCriterionStrings.size() - 1;
                    awardCriterionStrings.set(i + 1, awardCriterionStrings.get(i) + awardCriterionStrings.get(i + 1));
                    awardCriterionStrings.remove(i);
                    twoAwardCriteriaWasMerged = true;
                    break;
                }
            }
        } while (twoAwardCriteriaWasMerged);

        // The award criteria can be filled like this:
        // - "- prix : 40 % - délais : 30 % - valeur technique au vu d'un mémoire technique : 30 %."
        // - "- valeur technique : 40% - prix : 60%."
        // - "- la valeur technique des prestations au regard : - du contenu et de la qualité du mémoire explicatif
        //   ...
        //   références) ; : 60 % - le prix des prestations : 40 %."
        // So we parse it differently
        if (awardCriterionStrings.size() == 1) {
            int lastIndexOfDelimiter = awardCriterionStrings.get(0).lastIndexOf(nameWeightDelimiter);
            String weight = BOAMPTenderParserUtils.getNumberAtTheBeginning(
                awardCriterionStrings.get(0).substring(lastIndexOfDelimiter + 1));
            if (weight == null) {
                // The award criteria can be incorrectly filled:
                // "pour le lot no2 : installation et maintenance d'équipements radio antares et de leurs
                //  accessoires : 1. prix : 60 % - 2. note méthodologique détaillant :- les moyens mis en
                //  oeuvre - 20% et le délai d'installation - 20% - en cas de discordance constatée dans une offre,
                //  les indications portées sur le bordereau des prix unitaires prévaudront sur toutes autres
                //  indications de l'offre."
                // IMHO we are not able to automatically parse award criterion, so we save it as description
                awardCriteria.add(new ParsedAwardCriterion().setDescription(awardCriteriaString));
                return awardCriteria;
            }
            if (!weight.equals("100")) {
                awardCriterionStrings = new ArrayList<>(Arrays.asList(awardCriteriaString.split("% -")));
                for (String awardCriterionString : awardCriterionStrings) {
                    lastIndexOfDelimiter = awardCriterionString.lastIndexOf(nameWeightDelimiter);
                    if (lastIndexOfDelimiter == -1) {
                        // The award criteria can be incorrectly filled:
                        // "- valeur technique de l'offre - perception de la démarche 20% - méthodologie avec
                        //  production d'un mémoire technique détaillé argumenté et phasé 20% - références précisant
                        //  si le projet est abouti ou en cours d'étude et moyens mis en place pour cette
                        //  étude 20% : 60 % - prix - bordereau de décomposition de prix faisant apparaître le détail
                        //  par mission et par type d'intervenants 20 % - adéquation temps/mission 20 % : 40 %."
                        // IMHO we are not able to automatically parse award criterion, so we save it as description
                        return new ArrayList<>(Collections.singletonList(new ParsedAwardCriterion()
                            .setDescription(awardCriteriaString)));
                    }
                    weight = BOAMPTenderParserUtils.getNumberAtTheBeginning(awardCriterionString.substring(
                        lastIndexOfDelimiter + 1));
                    awardCriteria.add(new ParsedAwardCriterion()
                        .setName(awardCriterionString.substring(0, lastIndexOfDelimiter)).setWeight(weight));
                }
                return awardCriteria;
            }
        }

        for (String awardCriterionString : awardCriterionStrings) {
            // award criterion can be "- valeur technique de l'offre au vu du m�moire technique. cette valeur technique
            // sera not�e sur 20 avec la r�partition suivante : - m�thodologie de travail et moyens mis en oeuvre
            // (not�e sur 3) - qualit� de l'�preuve 0 (not�e sur 17) : 70 % "
            // The weight should be at the end separated by delimiter, so we want last index of the delimiter
            int lastIndexOfDelimiter = awardCriterionString.lastIndexOf(nameWeightDelimiter);
            if (lastIndexOfDelimiter != -1) {
                final String weight = BOAMPTenderParserUtils.getNumberAtTheBeginning(
                    awardCriterionString.substring(lastIndexOfDelimiter + 1));
                awardCriteria.add(new ParsedAwardCriterion()
                    .setName(awardCriterionString.substring(0, lastIndexOfDelimiter)).setWeight(weight));
            } else {
                // The award criteria can be incorrectly filled:
                // "- descriptif de la d�marche d'insertion : 60 % ; - co�t du dispositif 30 % ; - moyens associ�s au
                // d�veloppement durable 10 %."
                // IMHO we are not able to automatically parse award criterion, so we save it as description
                awardCriteria.add(new ParsedAwardCriterion().setDescription(awardCriteriaString));
                return awardCriteria;
            }
        }

        return awardCriteria;
    }

    /**
     * Parse tender bid deadline value from publication element.
     *
     * @param publicationElement
     *         publication element to be parsed
     *
     * @return String or Null
     */
    private static String parseTenderBidDeadline(final Element publicationElement) {
        /* Tender bid deadline is inside the DELAI element (date is in RECEPT_OFFRES and time is in the next
        element AUTRES, AUTRES is optional).
        There can be another dates:
          <delai>
            <recept_candidat>
              16 novembre 2012
            </recept_candidat>
            <autres>
              , � 16 heures
            </autres>
          </delai>

          <delai>
            <recept_offres>
              19 novembre 2012
            </recept_offres>
            <autres>
              , � 17 heures
            </autres>
            <validite_offre>
              120 jours
            </validite_offre>
            <autres>
              � compter de la date de reception des offres
            </autres>
          </delai>

          <delai>
            <recept_offres>
              8 août 2008
            </recept_offres>
            <validite_offre>
              90 jours
            </validite_offre>
            <autres>
              à compter de la date de reception des offres
            </autres>
          </delai>
        */
        final Elements bidDeadlineDateElements = JsoupUtils.select(publicationElement, "DONNEES > DELAI > RECEPT_OFFRES",
            " DONNEES > DELAI > RECEPT_CANDIDAT", "GESTION > K9");
        if (bidDeadlineDateElements == null || bidDeadlineDateElements.isEmpty()) {
            return null;
        }

        final Element bidDeadlineDateElement = bidDeadlineDateElements.get(0);
        final Element bidDeadlineTimeElement = bidDeadlineDateElement.nextElementSibling();
        return bidDeadlineTimeElement == null || !bidDeadlineTimeElement.tagName().equals("autres")
                ? bidDeadlineDateElement.text()
                : bidDeadlineDateElement.text() + " " +
                StringUtils.removeDotsAtTheEnd(bidDeadlineDateElement.nextElementSibling().text());
    }

    /**
     * Parse tender documents location from publication element.
     *
     * @param publicationElement
     *         publication element to be parsed
     *
     * @return award criterion list or Null
     */
    private static ParsedAddress parseTenderDocumentsLocation(final Element publicationElement) {
        Element documentsElement = JsoupUtils.selectFirst("DONNEES > DOCUMENTS:has(OBTENUS), CORRESPONDANTS > ADMIN_TECH:has(OBTENUS)",
            publicationElement);
        if (documentsElement == null) {
            return null;
        }

        return BOAMPTenderParserUtils.parseAddress(documentsElement);
    }
}
