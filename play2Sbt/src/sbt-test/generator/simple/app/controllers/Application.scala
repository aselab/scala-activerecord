package controllers

import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Hello, Scala ActiveRecord"))
  }

}
