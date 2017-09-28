package eu.digiwhist.worker.sk.master;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.matched.MatchedBody;

/**
 * This comparator helps to order UVO bodies from one group. How it works?
 * - if some of matched bodies is preferred (isPreferred=TRUE) then this is winner
 * - if all matched bodies are not preferred (isPreferred=FALSE) then
 *   - body with ICO and name wins
 *   - if we don't have both then body with ICO wins
 *   - otherwise we prefer record with name
 *   - if we can't decide by above-mentioned rules than record with more not null fields wins
 *
 * @param <T>
 *            items to be compared
 */
final class UvoBodyComparator<T extends MatchedBody> implements Comparator<T> {
    @Override
    public int compare(final T matchedBody1, final T matchedBody2) {
        assert matchedBody1.getIsPreferred() == null || !matchedBody1.getIsPreferred()
                || matchedBody2.getIsPreferred() == null || !matchedBody2.getIsPreferred()
                : "Two bodies can not be preferred!!!";

        int cleanBody1Score = getScore(matchedBody1);
        int cleanBody2Score = getScore(matchedBody2);

        if (cleanBody1Score < cleanBody2Score) {
            return -1;
        } else if (cleanBody1Score == cleanBody2Score) {
            return 0;
        } else {
            return 1;
        }
    }

    /**
     * @param cleanBody
     *            clean body to be scored
     *
     * @return score
     */
    private int getScore(final T cleanBody) {
        int score = 0;

        score += (cleanBody.getIsPreferred() == null || !cleanBody.getIsPreferred()) ? 0 : 400;

        List<BodyIdentifier> bodyOrganizationId = cleanBody.getBodyIds()
                .stream()
                .filter(body -> (body.getType() == BodyIdentifier.Type.ORGANIZATION_ID))
                .collect(Collectors.toList());
        if (!bodyOrganizationId.isEmpty()) {
            assert bodyOrganizationId.size() == 1 && bodyOrganizationId.get(0).getId() != null;
            score += 200;
        }

        score += (cleanBody.getName() == null) ? 0 : 100;

        score += (cleanBody.getAddress() == null) ? 0 : 1;
        score += (cleanBody.getBuyerType() == null) ? 0 : 1;
        score += (cleanBody.getContactName() == null) ? 0 : 1;
        score += (cleanBody.getContactPoint() == null) ? 0 : 1;
        score += (cleanBody.getEmail() == null) ? 0 : 1;
        score += (cleanBody.getGroupId() == null) ? 0 : 1;
        score += (cleanBody.getHash() == null) ? 0 : 1;
        score += (cleanBody.getIsLeader() == null) ? 0 : 1;
        score += (cleanBody.getIsPublic() == null) ? 0 : 1;
        score += (cleanBody.getIsSectoral() == null) ? 0 : 1;
        score += (cleanBody.getIsSme() == null) ? 0 : 1;
        score += (cleanBody.getIsSubsidized() == null) ? 0 : 1;
        score += (cleanBody.getMainActivities() == null) ? 0 : 1;
        score += (cleanBody.getMatchedBy() == null) ? 0 : 1;
        score += (cleanBody.getPhone() == null) ? 0 : 1;

        return score;
    }
}
