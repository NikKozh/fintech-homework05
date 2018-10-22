package fintech.homework05
import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}

class TweetAppSpec extends FlatSpec with Matchers {
  val storage = new InMemoryTweetStorage()
  val app = new TweetApi(storage)

  object TweetExamples {
    val simpleTweet          = Tweet("1", "user1",
                                     "Simple text without numbers and one hashtag tags\n#singlehashtag",
                                     List("singlehashtag"), Some(Instant.now), 0)

    val tweetWithNumbers     = Tweet("2", "user2",
                                     "More complex text with numbers 123 and 2 hash tags\n#hashtagone#2ndhashtag",
                                     List("hashtagone", "2ndhashtag"), Some(Instant.now), 0)

    val tweetWithoutHashTags = Tweet("3", "user3",
                                     "Tweet without hash tags at all",
                                     List.empty[String], Some(Instant.now), 0)

    val tweetWithEmptyHashTag = Tweet("4", "user4",
                                      "Tweet with empty hash tag by user mistake #",
                                      List.empty[String], Some(Instant.now), 0)

    val tweetWithTooLongText  = Tweet("5", "user5",
                                      "Tweet with too long text, containing actually more than 140 symbols." +
                                        "This tweet is invalid and must not be added to storage." +
                                        "TweetAPI must return Result[Error] on this tweet." +
                                        "#toolongtweet#nevergowithtoolongtweets",
                                      List("toolongtweet", "nevergowithtoolongtweets"), Some(Instant.now), 0)

    val tweetList = List(simpleTweet, tweetWithNumbers, tweetWithoutHashTags,
                         tweetWithEmptyHashTag, tweetWithTooLongText)
  }

  "getHashTags" should "return Seq of hash tags from text with hash tags" in {
    val resultList = TweetExamples.tweetList.map(tweet => app.getHashTags(tweet.text))

    TweetExamples.tweetList.zip(resultList).foreach(resultPair =>
      resultPair._1.hashTags should be(resultPair._2))
  }

  "getTweet" should "return exactly the same tweet, which was created with same id" in {
    val createdTweet = app.createTweet(CreateTweetRequest("Text of test tweet", "TestUser"))
    val gotTweet = app.getTweet(GetTweetRequest(createdTweet.get.id))
    createdTweet.get should be(gotTweet.get)
  }
}
