package fintech.homework05

import java.time.Instant
import java.util.UUID
import scala.collection.immutable.HashMap

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Result[T]
  * в котором может лежать либо текст ошибки, либо результат выполнения
  */

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int)

case class CreateTweetRequest(text: String, user: String)
case class GetTweetRequest(id: String)
case class LikeRequest(id: String)

trait TweetStorage {
  def saveTweet(tweet: Tweet): Result[Tweet]
  def getTweet(id: String): Result[Tweet]
}

final class InMemoryTweetStorage() extends TweetStorage {
  private var storage: Map[String, Tweet] = new HashMap()

  def saveTweet(tweet: Tweet): Result[Tweet] =
    if (storage.contains(tweet.id))
      Error(s"Tweet with id ${tweet.id} already exists in the storage!")
    else {
      storage = storage.updated(tweet.id, tweet)
      Success(tweet)
    }

  def getTweet(id: String): Result[Tweet] = storage.get(id) match {
    case Some(tweet) => Success(tweet)
    case None        => Error(s"Tweet with id $id not found in the storage!")
  }
}

class TweetApi(storage: TweetStorage) {
  ???
}

object TweetApiExample extends App {
  /*
  val storage: TweetStorage = ???
  val app = new TwitterApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val response = app.createTweet(request)
  response match {
    case Success(value) => println(s"Created tweet with id: ${value.id}")
    case Error(message) => println(s"Failed to create tweet: $message")
  }
  */
}
