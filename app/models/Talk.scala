package models

case class Author(name: Option[String], twitter: Option[String])

case class Talk(
  id: String,
  title: String,
  author: Author,
  videoUrls: Option[Seq[String]],
  slidesUrls: Option[Seq[String]])
