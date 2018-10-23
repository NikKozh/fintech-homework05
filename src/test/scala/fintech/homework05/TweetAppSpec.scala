package fintech.homework05

import org.scalatest.{FlatSpec, Matchers}

class TweetAppSpec extends FlatSpec with Matchers {
  val storage = new InMemoryTweetStorage()
  val app = new TweetApi(storage)

  object HashTagExamples {
    val simpleText             = "Simple text without numbers and one hashtag tags\n#singlehashtag"
    val textWithNumbers        = "More complex text with numbers 123 and 2 hash tags\n#firsthashtag#2ndhashtag"
    val textWithMiddleHashTags = "Text with hash tags #rightinthemiddle of text #andwithanotherone\n with line ending"
    val textWithoutHashTags    = "Tweet without hash tags at all"
    val textWithEmptyHashTag   = "Tweet with empty hash tag by user mistake #"
    
    val textList = List(simpleText, textWithNumbers, textWithMiddleHashTags, textWithoutHashTags, textWithEmptyHashTag)
  }

  "getHashTags" should "return correct Seq of hash tags from text with hash tags" in {
    val hashTagList = HashTagExamples.textList.map(text => TweetApi.getHashTags(text))
    val resultList = List(List("singlehashtag"),
                          List("firsthashtag", "2ndhashtag"),
                          List("rightinthemiddle", "andwithanotherone"),
                          Nil,
                          Nil)

    hashTagList.zip(resultList).foreach(resultPair =>
      resultPair._1 should be(resultPair._2))
  }

  // lazy здесь для того, чтобы проверить метод createTweet в соответствующем тесте
  lazy val createdTweet: Tweet = app.createTweet(CreateTweetRequest("Text of test tweet", "TestUser")).get
  
  behavior of "createTweet"
  
  it should "create tweet, save it to storage and return if length of tweet text <= 140" in {
    createdTweet.text should be("Text of test tweet")
    createdTweet.user should be("TestUser")
  }

  it should "return Error with corresponding description if length of tweet text > 140" in {
    val tooLongTweet =
      app.createTweet(CreateTweetRequest("Tweet with too long text, containing actually more than 140 symbols." +
                                         "This tweet is invalid and must not be added to storage." +
                                         "TweetAPI must return Result[Error] on this tweet." +
                                         "#toolongtweet#nevergowithtoolongtweets", "troll"))

    tooLongTweet should be(Error("API error: tweet has more than 140 symbols!"))
  }

  "getTweet" should "return exactly the same tweet, which was created with same id" in {
    val gotTweet = app.getTweet(GetTweetRequest(createdTweet.id))
    gotTweet.get should be(createdTweet)
  }

  "incrementLikes" should "increment tweet likes for 1 and return count of new likes" in {
    val oneLike = app.incrementLikes(LikeRequest(createdTweet.id)).get
    oneLike should be(1)

    val fiveLikes = (1 to 4).map(_ => app.incrementLikes(LikeRequest(createdTweet.id)).get).last
    fiveLikes should be(5)
  }

  "getTweet and incrementLikes" should
    "return Error with corresponding description if tweet with provided id not found" in {

    val nonexistentTweet      = app.getTweet(GetTweetRequest("-1"))
    val failedLikesIncrement  = app.incrementLikes(LikeRequest("-1"))
    val nonexistentTweetError = Error(s"Storage load error: tweet with id -1 not found in the storage!")

    nonexistentTweet     should be(nonexistentTweetError)
    failedLikesIncrement should be(nonexistentTweetError)
  }
}
