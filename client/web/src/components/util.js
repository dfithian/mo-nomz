import * as React from "react"
import { ContentRow } from "./row"

const Title = ({ text }) => {
  return (
    <h1 className="title is-2 has-text-centered">{text}</h1>
  )
}

const H1 = ({ text }) => {
  return (
    <h1 className="subtitle is-2">{text}</h1>
  )
}

const H2 = ({ text }) => {
  return (
    <h2 className="subtitle is-3">{text}</h2>
  )
}

const H3 = ({ text }) => {
  return (
    <h3 className="subtitle is-4">{text}</h3>
  )
}

const H4 = ({ text }) => {
  return (
    <h4 className="subtitle is-5">{text}</h4>
  )
}

const PlainImg = ({ image, size, alt }) => {
  return (
    <figure className={"image " + size}>
      <img src={image} alt={alt} />
    </figure>
  )
}

const Img = ({ image, size, alt }) => {
  return (
    <div className="container">
      <PlainImg image={image} size={size} alt={alt} />
    </div>
  )
}

const ImgLink = ({ href, image, size, alt }) => {
  return (
    <a href={href}>
      <Img image={image} size={size} alt={alt} />
    </a>
  )
}

const Msg = ({ children }) => {
  return (
    <ContentRow>
      <div className="box">
        {children}
      </div>
    </ContentRow>
  )
}

export {
  Title,
  H1,
  H2,
  H3,
  H4,
  PlainImg,
  Img,
  ImgLink,
  Msg,
}
