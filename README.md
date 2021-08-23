# AdaTip

Creator tipping service that is censorship-resistant and gives access to
exclusive content though a subscription-based monetization scheme.

## Highlights

 - Pay to subscribe to creators for exclusive content.
 - Tip creators using decentralized payment system.
 - Host your own instance and earn commission.

## Requirements

 - Creators can sign up with an instance of their choice.
 - Creators can log in to the instance using an authentication method.
   - TODO: Specify requirements for authentication method.
 - Creators can post content in the form of text, images, video, or audio.
 - Creators can configure tiers.
 - Creators can configure tip suggestions.
 - Anybody can obtaining a subscription, by:
    - Buying it from the creator through a smart contract.
      An instance-configured fee will go to the instance administrator.
    - Receiving it from somebody else (perhaps as part of a gift or trade).
 - Anybody can tip any creator, regardless of subscription status.
 - Anybody can become a subscriber by
   signing a nonce whilst owning a subscription.
    - Becoming a subscriber does not require signing up with an instance;
      the instance will simply ask to sign the nonce
      when visiting without a relevant cookie.
 - While in period P, owning a subscription (I, C, T, P)
   grants access to posts on instance I by creator C
   that the creator marked as exclusive to tier T.

## Definitions

<dl>

  <dt>Period</dt>
  <dd>Specific month, such as June 2022.</dd>

  <dt>Tip</dt>
  <dd>
    Coins sent directly to a creator.
    Not a feature of AdaTip per se,
    but see <em>tip suggestion</em>.
    Tips do not grant access to exclusive content.
  </dd>

  <dt>Tip suggestion</dt>
  <dd>
    Creator-determined (Cardano address, suggested amount) pair.
    Tip suggestions are independent of tiers.
  </dd>

  <dt>Subscriber</dt>
  <dd>
    Person that has proved that they own a subscription.
  </dd>

  <dt>Subscription</dt>
  <dd>
    Token representing a (instance, creator, tier, period) quadruplet.
    Subscriptions are independent of tips.
  </dd>

  <dt>Tier</dt>
  <dd>
    Creator-determined price for accessing a collection of posts.
    Tiers are independent of tip suggestions.
  </dd>

</dl>
