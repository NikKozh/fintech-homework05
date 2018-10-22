package fintech.homework05

import java.time.Instant
import org.scalatest.{FlatSpec, Matchers}

class TweetStorageSpec extends FlatSpec with Matchers {
  val testStorage: TweetStorage = new InMemoryTweetStorage()
  val testTweet: Tweet = Tweet("1", "user1",
                               "Simple text without numbers and one hashtag tags\n#singlehashtag",
                               List("singlehashtag"), Some(Instant.now), 0)

  behavior of "saveTweet"

  it should "put provided tweet in the Map and return saved instance as Success " +
            "if tweet with same id was not found" in {
    testStorage.saveTweet(testTweet) should be(Success(testTweet))
  }

  it should "return Error with corresponding description if provided tweet has the same id as one of existing" in {
    testStorage.saveTweet(testTweet) should be(Error(
      s"Storage save error: tweet with id 1 already exists in the storage!"))
  }

  behavior of "loadTweet"

  it should "get tweet with provided id from the Map and return its instance as Success if such was found" in {
    testStorage.loadTweet("1") should be(Success(testTweet))
  }

  it should "return Error with corresponding description if provided tweet was not found in the Map" in {
    testStorage.loadTweet("2") should be(Error("Storage load error: tweet with id 2 not found in the storage!"))
  }

  behavior of "updateTweet"

  it should "change tweet with provided id in the Map according to other arguments " +
            "and return Success with its instance if such tweet was found" in {
    testStorage.updateTweet("1", None, None, Some(5)).get.likes should be(5)
  }

  it should "return Error with corresponding description if tweet with provided id was not found in the Map" in {
    testStorage.updateTweet("2", None, None, Some(10)) should be(Error(
      "Storage update error: tweet with id 2 not found in the storage!"))
  }
}
