import * as React from "react"

const Title = ({ text }) => {
  return (
    <h1 className="title is-2 has-text-centered">{text}</h1>
  )
}

const H1 = ({ children }) => {
  return (
    <h1 className="subtitle is-2">{children}</h1>
  )
}

const H2 = ({ children }) => {
  return (
    <h2 className="subtitle is-3">{children}</h2>
  )
}

const H3 = ({ children }) => {
  return (
    <h3 className="subtitle is-4">{children}</h3>
  )
}

const H4 = ({ children }) => {
  return (
    <h4 className="subtitle is-5">{children}</h4>
  )
}

const H5 = ({ children }) => {
  return (
    <h4 className="subtitle is-6">{children}</h4>
  )
}

const PlainImg = ({ image, size, alt }) => {
  return (
    <figure className={"image " + size}>
      <img src={image} alt={alt} />
    </figure>
  )
}

const ImgLink = ({ href, image, size, alt }) => {
  return (
    <a href={href}>
      <PlainImg image={image} size={size} alt={alt} />
    </a>
  )
}

const Centered = ({ children }) => {
  return (
    <div className="container">
      {children}
    </div>
  )
}

export {
  Title,
  H1,
  H2,
  H3,
  H4,
  H5,
  PlainImg,
  ImgLink,
  Centered,
}
